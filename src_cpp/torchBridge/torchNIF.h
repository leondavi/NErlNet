#pragma once

#include "NerlWorkerTorch.h"
#include "NerlWorkerTorchNIF.h"
#include "TensorCodec.h"
#include "bridgeController.h"

#include <chrono>
#include <cmath>
#include <cstring>
#include <type_traits>
#include <vector>

namespace
{

template<typename ValueType>
bool term_to_value(ErlNifEnv* env, ERL_NIF_TERM term, ValueType& out)
{
    double as_double = 0.0;
    if (enif_get_double(env, term, &as_double))
    {
        out = static_cast<ValueType>(as_double);
        return true;
    }

    long as_long = 0;
    if (enif_get_long(env, term, &as_long))
    {
        out = static_cast<ValueType>(as_long);
        return true;
    }

    return false;
}

template<typename ValueType>
bool collect_nerltensor_values(ErlNifEnv* env, ERL_NIF_TERM list_term, std::vector<ValueType>& out)
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = list_term;
    while (enif_get_list_cell(env, tail, &head, &tail))
    {
        ValueType value;
        if (!term_to_value(env, head, value))
        {
            return false;
        }
        out.push_back(value);
    }
    return enif_is_empty_list(env, tail);
}

template<typename ValueType>
bool validate_dimensions(const std::vector<ValueType>& values)
{
    if (values.size() < static_cast<size_t>(nerlnet::DIMS_TOTAL))
    {
        return false;
    }
    const double dimx = static_cast<double>(values[nerlnet::DIMS_X_IDX]);
    const double dimy = static_cast<double>(values[nerlnet::DIMS_Y_IDX]);
    const double dimz = static_cast<double>(values[nerlnet::DIMS_Z_IDX]);
    if (dimx <= 0.0 || dimy <= 0.0 || dimz <= 0.0)
    {
        return false;
    }
    const size_t expected = static_cast<size_t>(std::llround(dimx * dimy * dimz));
    const size_t actual = values.size() - nerlnet::DIMS_TOTAL;
    return expected == actual;
}

template<typename ValueType>
ERL_NIF_TERM make_value_term(ErlNifEnv* env, ValueType value)
{
    if constexpr (std::is_floating_point_v<ValueType>)
    {
        return enif_make_double(env, static_cast<double>(value));
    }
    return enif_make_int64(env, static_cast<long long>(value));
}

template<typename ValueType>
ERL_NIF_TERM encode_from_list(ErlNifEnv* env, ERL_NIF_TERM list_term, ERL_NIF_TERM type_term)
{
    unsigned int list_length = 0;
    if (!enif_get_list_length(env, list_term, &list_length))
    {
        return enif_make_badarg(env);
    }

    std::vector<ValueType> values;
    values.reserve(list_length);
    if (!collect_nerltensor_values(env, list_term, values) || !validate_dimensions(values))
    {
        return enif_make_badarg(env);
    }

    nifpp::binary buffer(values.size() * sizeof(ValueType));
    std::memcpy(buffer.data, values.data(), buffer.size);
    ERL_NIF_TERM binary_term = nifpp::make(env, buffer);
    return enif_make_tuple2(env, binary_term, type_term);
}

template<typename ValueType>
ERL_NIF_TERM decode_to_list(ErlNifEnv* env, ERL_NIF_TERM binary_term, ERL_NIF_TERM list_type_term)
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, binary_term, &bin))
    {
        return enif_make_badarg(env);
    }
    if (bin.size % sizeof(ValueType) != 0)
    {
        return enif_make_badarg(env);
    }

    const size_t total_elements = bin.size / sizeof(ValueType);
    std::vector<ValueType> values(total_elements);
    std::memcpy(values.data(), bin.data, bin.size);

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (auto it = values.rbegin(); it != values.rend(); ++it)
    {
        list = enif_make_list_cell(env, make_value_term(env, *it), list);
    }

    return enif_make_tuple2(env, list, list_type_term);
}

ERL_NIF_TERM list_type_from_dtype(ErlNifEnv* env, c10::ScalarType dtype)
{
    if (dtype == c10::ScalarType::Float || dtype == c10::ScalarType::Double)
    {
        return nifpp::make(env, nifpp::str_atom("erl_float"));
    }
    return nifpp::make(env, nifpp::str_atom("erl_int"));
}

nerlnet::TorchTensor ensure_tensor_dtype(const nerlnet::TorchTensor &tensor, c10::ScalarType dtype)
{
    if (!tensor.defined())
    {
        return tensor;
    }
    if (tensor.scalar_type() == dtype)
    {
        return tensor;
    }
    return tensor.to(dtype);
}

ERL_NIF_TERM make_pipeline_error(ErlNifEnv* env, const std::string& message)
{
    ERL_NIF_TERM nerlnif_atom = enif_make_atom(env, "nerlnif");
    nifpp::TERM error_atom = nifpp::make(env, nifpp::str_atom("error"));
    nifpp::TERM msg_term = nifpp::make(env, message);
    return enif_make_tuple(env, 3, nerlnif_atom, error_atom, msg_term);
}

} // namespace

class dirty_thread_args
{
public:
    long int mid; // model id
    nerlnet::TorchTensor nerltensor;
    nifpp::str_atom return_tensor_type; // holds the type of tensor should be returned
    c10::ScalarType tensor_dtype;
    std::chrono::high_resolution_clock::time_point start_time;
    long microbatch_id{-1};
    bool is_microbatch{false};


    ErlNifTid tid;
    ErlNifPid pid;
};

void* train_threaded_function(void* args);
void* predict_threaded_function(void* args);

/*
train_nif function is called by NIF from Erlang. 
It creates a TorchTensor from input data and calls the threaded train funciton
*/
static ERL_NIF_TERM train_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::shared_ptr<dirty_thread_args>* p_thread_args_ptr = new std::shared_ptr<dirty_thread_args>(std::make_shared<dirty_thread_args>());
    std::shared_ptr<dirty_thread_args> thread_args_ptr = *p_thread_args_ptr;

    nifpp::str_atom tensor_type;
    c10::ScalarType torch_dtype;

    thread_args_ptr->start_time = std::chrono::high_resolution_clock::now();
    thread_args_ptr->is_microbatch = false;
    thread_args_ptr->microbatch_id = -1;

    enum{ARG_MODEL_ID, ARG_NERLTENSOR, ARG_NERLTENSOR_TYPE};

    nifpp::get_throws(env, argv[ARG_NERLTENSOR_TYPE],tensor_type);
    thread_args_ptr->return_tensor_type = tensor_type;
    // extract model id
    nifpp::get_throws(env, argv[ARG_MODEL_ID], thread_args_ptr->mid);
    torch_dtype = nerlnet::get_torch_dtype(tensor_type);
    thread_args_ptr->tensor_dtype = torch_dtype;
    // extract nerltensor
    thread_args_ptr->nerltensor = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_NERLTENSOR], torch_dtype);

    ErlNifPid pid;
    enif_self(env, &pid);
    thread_args_ptr->pid = pid;

    char thread_name[] = "train_thread";
    int thread_create_status = enif_thread_create(thread_name, &(thread_args_ptr->tid), train_threaded_function, (void*) p_thread_args_ptr, NULL);
    void* exit_code = nullptr;
    if (thread_create_status != 0)
    {
        LogError("failed to call enif_thread_create with train_nif");
        nifpp::str_atom ret_status("train_nif_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        thread_create_status = enif_thread_join(thread_args_ptr->tid, &exit_code );
        if (thread_create_status != 0)
        {
            LogError("failed to join with train_nif");
            nifpp::str_atom ret_status("train_nif_error");
            return nifpp::make(env, ret_status);
        }
    }
    nifpp::str_atom ret_status("ok");
    return nifpp::make(env, ret_status);
}

static ERL_NIF_TERM train_microbatch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::shared_ptr<dirty_thread_args>* p_thread_args_ptr = new std::shared_ptr<dirty_thread_args>(std::make_shared<dirty_thread_args>());
    std::shared_ptr<dirty_thread_args> thread_args_ptr = *p_thread_args_ptr;

    nifpp::str_atom tensor_type;
    c10::ScalarType torch_dtype;
    long microbatch_id = 0;

    thread_args_ptr->start_time = std::chrono::high_resolution_clock::now();
    thread_args_ptr->is_microbatch = true;

    enum{ARG_MODEL_ID, ARG_NERLTENSOR, ARG_NERLTENSOR_TYPE, ARG_MICROBATCH_ID};

    nifpp::get_throws(env, argv[ARG_NERLTENSOR_TYPE], tensor_type);
    thread_args_ptr->return_tensor_type = tensor_type;
    nifpp::get_throws(env, argv[ARG_MODEL_ID], thread_args_ptr->mid);
    if (!enif_get_long(env, argv[ARG_MICROBATCH_ID], &microbatch_id))
    {
        delete p_thread_args_ptr;
        return enif_make_badarg(env);
    }
    thread_args_ptr->microbatch_id = microbatch_id;
    torch_dtype = nerlnet::get_torch_dtype(tensor_type);
    thread_args_ptr->tensor_dtype = torch_dtype;
    thread_args_ptr->nerltensor = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_NERLTENSOR], torch_dtype);

    ErlNifPid pid;
    enif_self(env, &pid);
    thread_args_ptr->pid = pid;

    char thread_name[] = "train_microbatch_thread";
    int thread_create_status = enif_thread_create(thread_name, &(thread_args_ptr->tid), train_threaded_function, (void*) p_thread_args_ptr, NULL);
    void* exit_code = nullptr;
    if (thread_create_status != 0)
    {
        LogError("failed to call enif_thread_create with train_microbatch_nif");
        nifpp::str_atom ret_status("train_microbatch_nif_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        thread_create_status = enif_thread_join(thread_args_ptr->tid, &exit_code );
        if (thread_create_status != 0)
        {
            LogError("failed to join with train_microbatch_nif");
            nifpp::str_atom ret_status("train_microbatch_nif_error");
            return nifpp::make(env, ret_status);
        }
    }
    nifpp::str_atom ret_status("ok");
    return nifpp::make(env, ret_status);
}

static ERL_NIF_TERM optimizer_barrier_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_MODEL_ID};
    nifpp::str_atom ok_atom("ok");
    nifpp::str_atom error_atom("error");

    try
    {
        unsigned long model_id = 0;
        nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
        nerlnet::BridgeController& controller = nerlnet::BridgeController::GetInstance();
        std::shared_ptr<nerlnet::NerlWorker> worker_base = controller.getModelPtr(model_id);
        std::shared_ptr<nerlnet::NerlWorkerTorch> worker = std::dynamic_pointer_cast<nerlnet::NerlWorkerTorch>(worker_base);
        if (!worker)
        {
            return nifpp::make(env, error_atom);
        }
        worker->optimizer_barrier();
        return nifpp::make(env, ok_atom);
    }
    catch (const std::exception& ex)
    {
        LogError << "optimizer_barrier_nif failed: " << ex.what() << std::endl;
        return nifpp::make(env, error_atom);
    }
    catch (...)
    {
        LogError << "optimizer_barrier_nif failed with unknown error" << std::endl;
        return nifpp::make(env, error_atom);
    }
}

static ERL_NIF_TERM pipeline_stage0_forward_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_MODEL_ID, ARG_NERLTENSOR, ARG_NERLTENSOR_TYPE, ARG_BATCH_ID, ARG_MICROBATCH_ID};
    nifpp::str_atom ok_atom("ok");
    nifpp::str_atom stage_atom("pipeline_stage0_forward");

    try
    {
        unsigned long model_id = 0;
        long batch_id = 0;
        long microbatch_id = 0;
        nifpp::str_atom tensor_type;
        nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
        nifpp::get_throws(env, argv[ARG_NERLTENSOR_TYPE], tensor_type);
        if (!enif_get_long(env, argv[ARG_BATCH_ID], &batch_id))
        {
            return enif_make_badarg(env);
        }
        if (!enif_get_long(env, argv[ARG_MICROBATCH_ID], &microbatch_id))
        {
            return enif_make_badarg(env);
        }

        const c10::ScalarType dtype = nerlnet::get_torch_dtype(tensor_type);
        const auto stage_input = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_NERLTENSOR], dtype);
        const auto start = std::chrono::high_resolution_clock::now();

        nerlnet::BridgeController& controller = nerlnet::BridgeController::GetInstance();
        std::shared_ptr<nerlnet::NerlWorker> worker_base = controller.getModelPtr(model_id);
        std::shared_ptr<nerlnet::NerlWorkerTorch> worker = std::dynamic_pointer_cast<nerlnet::NerlWorkerTorch>(worker_base);
        if (!worker)
        {
            return make_pipeline_error(env, "pipeline_stage0_forward_missing_worker");
        }

        auto [activation, labels] = worker->pipeline_stage0_forward(stage_input, batch_id, microbatch_id);
        activation = ensure_tensor_dtype(activation, dtype);
        labels = ensure_tensor_dtype(labels, dtype);

        nifpp::TERM encoded_activation;
        nifpp::TERM encoded_labels;
        nerlnet::torchbridge::TensorCodec::encode(env, activation, encoded_activation);
        nerlnet::torchbridge::TensorCodec::encode(env, labels, encoded_labels);

        const auto stop = std::chrono::high_resolution_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
        ERL_NIF_TERM stage_time = enif_make_double(env, static_cast<double>(duration.count()));

        return enif_make_tuple(env,
                               7,
                               nifpp::make(env, ok_atom),
                               nifpp::make(env, stage_atom),
                               encoded_activation,
                               nifpp::make(env, tensor_type),
                               encoded_labels,
                               nifpp::make(env, tensor_type),
                               stage_time);
    }
    catch (const std::exception& ex)
    {
        LogError << "pipeline_stage0_forward_nif failed: " << ex.what() << std::endl;
        return make_pipeline_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "pipeline_stage0_forward_nif failed with unknown error" << std::endl;
        return make_pipeline_error(env, "pipeline_stage0_forward_unknown_error");
    }
}

static ERL_NIF_TERM pipeline_stage_forward_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum
    {
        ARG_MODEL_ID,
        ARG_ACTIVATION_TENSOR,
        ARG_ACTIVATION_TYPE,
        ARG_LABELS_TENSOR,
        ARG_LABELS_TYPE,
        ARG_BATCH_ID,
        ARG_MICROBATCH_ID
    };
    nifpp::str_atom ok_atom("ok");
    nifpp::str_atom stage_atom("pipeline_stage_forward");

    try
    {
        unsigned long model_id = 0;
        long batch_id = 0;
        long microbatch_id = 0;
        nifpp::str_atom activation_type;
        nifpp::str_atom labels_type;
        nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
        nifpp::get_throws(env, argv[ARG_ACTIVATION_TYPE], activation_type);
        nifpp::get_throws(env, argv[ARG_LABELS_TYPE], labels_type);
        if (!enif_get_long(env, argv[ARG_BATCH_ID], &batch_id))
        {
            return enif_make_badarg(env);
        }
        if (!enif_get_long(env, argv[ARG_MICROBATCH_ID], &microbatch_id))
        {
            return enif_make_badarg(env);
        }

        const c10::ScalarType activation_dtype = nerlnet::get_torch_dtype(activation_type);
        const c10::ScalarType labels_dtype = nerlnet::get_torch_dtype(labels_type);
        const auto activation = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_ACTIVATION_TENSOR], activation_dtype);
        const auto labels = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_LABELS_TENSOR], labels_dtype);
        const auto start = std::chrono::high_resolution_clock::now();

        nerlnet::BridgeController& controller = nerlnet::BridgeController::GetInstance();
        std::shared_ptr<nerlnet::NerlWorker> worker_base = controller.getModelPtr(model_id);
        std::shared_ptr<nerlnet::NerlWorkerTorch> worker = std::dynamic_pointer_cast<nerlnet::NerlWorkerTorch>(worker_base);
        if (!worker)
        {
            return make_pipeline_error(env, "pipeline_stage_forward_missing_worker");
        }

        auto [stage_output, forwarded_labels] = worker->pipeline_stage_forward(activation, labels, batch_id, microbatch_id);
        stage_output = ensure_tensor_dtype(stage_output, activation_dtype);
        forwarded_labels = ensure_tensor_dtype(forwarded_labels, labels_dtype);

        nifpp::TERM encoded_stage_output;
        nifpp::TERM encoded_forwarded_labels;
        nerlnet::torchbridge::TensorCodec::encode(env, stage_output, encoded_stage_output);
        nerlnet::torchbridge::TensorCodec::encode(env, forwarded_labels, encoded_forwarded_labels);

        const auto stop = std::chrono::high_resolution_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
        ERL_NIF_TERM stage_time = enif_make_double(env, static_cast<double>(duration.count()));

        return enif_make_tuple(env,
                               7,
                               nifpp::make(env, ok_atom),
                               nifpp::make(env, stage_atom),
                               encoded_stage_output,
                               nifpp::make(env, activation_type),
                               encoded_forwarded_labels,
                               nifpp::make(env, labels_type),
                               stage_time);
    }
    catch (const std::exception& ex)
    {
        LogError << "pipeline_stage_forward_nif failed: " << ex.what() << std::endl;
        return make_pipeline_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "pipeline_stage_forward_nif failed with unknown error" << std::endl;
        return make_pipeline_error(env, "pipeline_stage_forward_unknown_error");
    }
}

static ERL_NIF_TERM pipeline_stage_last_forward_backward_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum
    {
        ARG_MODEL_ID,
        ARG_ACTIVATION_TENSOR,
        ARG_ACTIVATION_TYPE,
        ARG_LABELS_TENSOR,
        ARG_LABELS_TYPE,
        ARG_BATCH_ID,
        ARG_MICROBATCH_ID
    };
    nifpp::str_atom ok_atom("ok");
    nifpp::str_atom stage_atom("pipeline_stage_last");

    try
    {
        unsigned long model_id = 0;
        long batch_id = 0;
        long microbatch_id = 0;
        nifpp::str_atom activation_type;
        nifpp::str_atom labels_type;
        nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
        nifpp::get_throws(env, argv[ARG_ACTIVATION_TYPE], activation_type);
        nifpp::get_throws(env, argv[ARG_LABELS_TYPE], labels_type);
        if (!enif_get_long(env, argv[ARG_BATCH_ID], &batch_id))
        {
            return enif_make_badarg(env);
        }
        if (!enif_get_long(env, argv[ARG_MICROBATCH_ID], &microbatch_id))
        {
            return enif_make_badarg(env);
        }

        const c10::ScalarType activation_dtype = nerlnet::get_torch_dtype(activation_type);
        const c10::ScalarType labels_dtype = nerlnet::get_torch_dtype(labels_type);
        const auto activation = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_ACTIVATION_TENSOR], activation_dtype);
        const auto labels = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_LABELS_TENSOR], labels_dtype);
        const auto start = std::chrono::high_resolution_clock::now();

        nerlnet::BridgeController& controller = nerlnet::BridgeController::GetInstance();
        std::shared_ptr<nerlnet::NerlWorker> worker_base = controller.getModelPtr(model_id);
        std::shared_ptr<nerlnet::NerlWorkerTorch> worker = std::dynamic_pointer_cast<nerlnet::NerlWorkerTorch>(worker_base);
        if (!worker)
        {
            return make_pipeline_error(env, "pipeline_stage_last_missing_worker");
        }

        auto [loss_tensor, grad_input] = worker->pipeline_stage_last_forward_backward(activation, labels, batch_id, microbatch_id);
        loss_tensor = ensure_tensor_dtype(loss_tensor, labels_dtype);
        grad_input = ensure_tensor_dtype(grad_input, activation_dtype);

        nifpp::TERM encoded_loss;
        nifpp::TERM encoded_grad_input;
        nerlnet::torchbridge::TensorCodec::encode(env, loss_tensor, encoded_loss);
        nerlnet::torchbridge::TensorCodec::encode(env, grad_input, encoded_grad_input);

        const auto stop = std::chrono::high_resolution_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
        ERL_NIF_TERM stage_time = enif_make_double(env, static_cast<double>(duration.count()));

        return enif_make_tuple(env,
                               7,
                               nifpp::make(env, ok_atom),
                               nifpp::make(env, stage_atom),
                               encoded_loss,
                               nifpp::make(env, labels_type),
                               encoded_grad_input,
                               nifpp::make(env, activation_type),
                               stage_time);
    }
    catch (const std::exception& ex)
    {
        LogError << "pipeline_stage_last_forward_backward_nif failed: " << ex.what() << std::endl;
        return make_pipeline_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "pipeline_stage_last_forward_backward_nif failed with unknown error" << std::endl;
        return make_pipeline_error(env, "pipeline_stage_last_forward_backward_unknown_error");
    }
}

static ERL_NIF_TERM pipeline_stage_backward_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_MODEL_ID, ARG_GRAD_TENSOR, ARG_GRAD_TYPE, ARG_BATCH_ID, ARG_MICROBATCH_ID};
    nifpp::str_atom ok_atom("ok");
    nifpp::str_atom stage_atom("pipeline_stage_backward");

    try
    {
        unsigned long model_id = 0;
        long batch_id = 0;
        long microbatch_id = 0;
        nifpp::str_atom grad_type;
        nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
        nifpp::get_throws(env, argv[ARG_GRAD_TYPE], grad_type);
        if (!enif_get_long(env, argv[ARG_BATCH_ID], &batch_id))
        {
            return enif_make_badarg(env);
        }
        if (!enif_get_long(env, argv[ARG_MICROBATCH_ID], &microbatch_id))
        {
            return enif_make_badarg(env);
        }

        const c10::ScalarType grad_dtype = nerlnet::get_torch_dtype(grad_type);
        const auto grad_output = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_GRAD_TENSOR], grad_dtype);
        const auto start = std::chrono::high_resolution_clock::now();

        nerlnet::BridgeController& controller = nerlnet::BridgeController::GetInstance();
        std::shared_ptr<nerlnet::NerlWorker> worker_base = controller.getModelPtr(model_id);
        std::shared_ptr<nerlnet::NerlWorkerTorch> worker = std::dynamic_pointer_cast<nerlnet::NerlWorkerTorch>(worker_base);
        if (!worker)
        {
            return make_pipeline_error(env, "pipeline_stage_backward_missing_worker");
        }

        auto grad_input = worker->pipeline_stage_backward(grad_output, batch_id, microbatch_id);
        grad_input = ensure_tensor_dtype(grad_input, grad_dtype);

        nifpp::TERM encoded_grad_input;
        nerlnet::torchbridge::TensorCodec::encode(env, grad_input, encoded_grad_input);

        const auto stop = std::chrono::high_resolution_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
        ERL_NIF_TERM stage_time = enif_make_double(env, static_cast<double>(duration.count()));

        return enif_make_tuple(env,
                               5,
                               nifpp::make(env, ok_atom),
                               nifpp::make(env, stage_atom),
                               encoded_grad_input,
                               nifpp::make(env, grad_type),
                               stage_time);
    }
    catch (const std::exception& ex)
    {
        LogError << "pipeline_stage_backward_nif failed: " << ex.what() << std::endl;
        return make_pipeline_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "pipeline_stage_backward_nif failed with unknown error" << std::endl;
        return make_pipeline_error(env, "pipeline_stage_backward_unknown_error");
    }
}

static ERL_NIF_TERM pipeline_predict_stage0_forward_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_MODEL_ID, ARG_NERLTENSOR, ARG_NERLTENSOR_TYPE};
    nifpp::str_atom ok_atom("ok");
    nifpp::str_atom stage_atom("pipeline_predict_stage0");

    try
    {
        unsigned long model_id = 0;
        nifpp::str_atom tensor_type;
        nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
        nifpp::get_throws(env, argv[ARG_NERLTENSOR_TYPE], tensor_type);
        const c10::ScalarType dtype = nerlnet::get_torch_dtype(tensor_type);
        const auto stage_input = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_NERLTENSOR], dtype);
        const auto start = std::chrono::high_resolution_clock::now();

        nerlnet::BridgeController& controller = nerlnet::BridgeController::GetInstance();
        std::shared_ptr<nerlnet::NerlWorker> worker_base = controller.getModelPtr(model_id);
        std::shared_ptr<nerlnet::NerlWorkerTorch> worker = std::dynamic_pointer_cast<nerlnet::NerlWorkerTorch>(worker_base);
        if (!worker)
        {
            return make_pipeline_error(env, "pipeline_predict_stage0_missing_worker");
        }

        auto stage_output = worker->pipeline_predict_stage0_forward(stage_input);
        stage_output = ensure_tensor_dtype(stage_output, dtype);

        nifpp::TERM encoded_stage_output;
        nerlnet::torchbridge::TensorCodec::encode(env, stage_output, encoded_stage_output);

        const auto stop = std::chrono::high_resolution_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
        ERL_NIF_TERM stage_time = enif_make_double(env, static_cast<double>(duration.count()));

        return enif_make_tuple(env,
                               5,
                               nifpp::make(env, ok_atom),
                               nifpp::make(env, stage_atom),
                               encoded_stage_output,
                               nifpp::make(env, tensor_type),
                               stage_time);
    }
    catch (const std::exception& ex)
    {
        LogError << "pipeline_predict_stage0_forward_nif failed: " << ex.what() << std::endl;
        return make_pipeline_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "pipeline_predict_stage0_forward_nif failed with unknown error" << std::endl;
        return make_pipeline_error(env, "pipeline_predict_stage0_forward_unknown_error");
    }
}

static ERL_NIF_TERM pipeline_predict_stage_forward_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_MODEL_ID, ARG_NERLTENSOR, ARG_NERLTENSOR_TYPE};
    nifpp::str_atom ok_atom("ok");
    nifpp::str_atom stage_atom("pipeline_predict_stage");

    try
    {
        unsigned long model_id = 0;
        nifpp::str_atom tensor_type;
        nifpp::get_throws(env, argv[ARG_MODEL_ID], model_id);
        nifpp::get_throws(env, argv[ARG_NERLTENSOR_TYPE], tensor_type);
        const c10::ScalarType dtype = nerlnet::get_torch_dtype(tensor_type);
        const auto stage_input = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_NERLTENSOR], dtype);
        const auto start = std::chrono::high_resolution_clock::now();

        nerlnet::BridgeController& controller = nerlnet::BridgeController::GetInstance();
        std::shared_ptr<nerlnet::NerlWorker> worker_base = controller.getModelPtr(model_id);
        std::shared_ptr<nerlnet::NerlWorkerTorch> worker = std::dynamic_pointer_cast<nerlnet::NerlWorkerTorch>(worker_base);
        if (!worker)
        {
            return make_pipeline_error(env, "pipeline_predict_stage_missing_worker");
        }

        auto stage_output = worker->pipeline_predict_stage_forward(stage_input);
        stage_output = ensure_tensor_dtype(stage_output, dtype);

        nifpp::TERM encoded_stage_output;
        nerlnet::torchbridge::TensorCodec::encode(env, stage_output, encoded_stage_output);

        const auto stop = std::chrono::high_resolution_clock::now();
        const auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
        ERL_NIF_TERM stage_time = enif_make_double(env, static_cast<double>(duration.count()));

        return enif_make_tuple(env,
                               5,
                               nifpp::make(env, ok_atom),
                               nifpp::make(env, stage_atom),
                               encoded_stage_output,
                               nifpp::make(env, tensor_type),
                               stage_time);
    }
    catch (const std::exception& ex)
    {
        LogError << "pipeline_predict_stage_forward_nif failed: " << ex.what() << std::endl;
        return make_pipeline_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "pipeline_predict_stage_forward_nif failed with unknown error" << std::endl;
        return make_pipeline_error(env, "pipeline_predict_stage_forward_unknown_error");
    }
}
/*
predict_nif function is called by NIF from Erlang.
It creates a TorchTensor from input data and calls the threaded predict function
*/
static ERL_NIF_TERM predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ 
    std::shared_ptr<dirty_thread_args>* p_thread_args_ptr = new std::shared_ptr<dirty_thread_args>(std::make_shared<dirty_thread_args>());
    std::shared_ptr<dirty_thread_args> thread_args_ptr = *p_thread_args_ptr;

    nifpp::str_atom tensor_type;
    c10::ScalarType torch_dtype;

    thread_args_ptr->start_time = std::chrono::high_resolution_clock::now();

        enum{ARG_MODEL_ID, ARG_NERLTENSOR, ARG_NERLTENSOR_TYPE};

    nifpp::get_throws(env, argv[ARG_NERLTENSOR_TYPE],tensor_type);
    thread_args_ptr->return_tensor_type = tensor_type;
    // extract model id
    nifpp::get_throws(env, argv[ARG_MODEL_ID], thread_args_ptr->mid);
    torch_dtype = nerlnet::get_torch_dtype(tensor_type);
    thread_args_ptr->tensor_dtype = torch_dtype;
    // extract nerltensor
    thread_args_ptr->nerltensor = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_NERLTENSOR], torch_dtype);

    ErlNifPid pid;
    enif_self(env, &pid);
    thread_args_ptr->pid = pid;

    char thread_name[] = "predict_thread";
    int thread_create_status = enif_thread_create(thread_name, &(thread_args_ptr->tid), predict_threaded_function, (void*) p_thread_args_ptr, NULL);
    void* exit_code = nullptr;
    if (thread_create_status != 0)
    {
        LogError("failed to call enif_thread_create with predict_nif");
        nifpp::str_atom ret_status("predict_nif_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        thread_create_status = enif_thread_join(thread_args_ptr->tid, &exit_code );
        if (thread_create_status != 0)
        {
            LogError("failed to join with predict_nif");
            nifpp::str_atom ret_status("predict_nif_error");
            return nifpp::make(env, ret_status);
        }
    }
    nifpp::str_atom ret_status("ok");
    return nifpp::make(env, ret_status);
}

static ERL_NIF_TERM encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_NERLTENSOR_LIST, ARG_BINARY_TYPE};
    nifpp::str_atom binary_type;
    nifpp::get_throws(env, argv[ARG_BINARY_TYPE], binary_type);
    c10::ScalarType dtype = nerlnet::get_torch_dtype(binary_type);

    switch (dtype)
    {
        case c10::ScalarType::Float:
            return encode_from_list<float>(env, argv[ARG_NERLTENSOR_LIST], argv[ARG_BINARY_TYPE]);
        case c10::ScalarType::Double:
            return encode_from_list<double>(env, argv[ARG_NERLTENSOR_LIST], argv[ARG_BINARY_TYPE]);
        case c10::ScalarType::Int:
            return encode_from_list<int32_t>(env, argv[ARG_NERLTENSOR_LIST], argv[ARG_BINARY_TYPE]);
        case c10::ScalarType::Short:
            return encode_from_list<int16_t>(env, argv[ARG_NERLTENSOR_LIST], argv[ARG_BINARY_TYPE]);
        default:
            return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_BINARY, ARG_BINARY_TYPE};
    nifpp::str_atom binary_type;
    nifpp::get_throws(env, argv[ARG_BINARY_TYPE], binary_type);
    c10::ScalarType dtype = nerlnet::get_torch_dtype(binary_type);
    ERL_NIF_TERM list_type_term = list_type_from_dtype(env, dtype);

    switch (dtype)
    {
        case c10::ScalarType::Float:
            return decode_to_list<float>(env, argv[ARG_BINARY], list_type_term);
        case c10::ScalarType::Double:
            return decode_to_list<double>(env, argv[ARG_BINARY], list_type_term);
        case c10::ScalarType::Int:
            return decode_to_list<int32_t>(env, argv[ARG_BINARY], list_type_term);
        case c10::ScalarType::Short:
            return decode_to_list<int16_t>(env, argv[ARG_BINARY], list_type_term);
        default:
            return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM nerltensor_sum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_BINARY_A, ARG_BINARY_B, ARG_BINARY_TYPE};
    nifpp::str_atom binary_type;
    nifpp::get_throws(env, argv[ARG_BINARY_TYPE], binary_type);
    c10::ScalarType dtype = nerlnet::get_torch_dtype(binary_type);

    nerlnet::TorchTensor tensor_a = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_BINARY_A], dtype);
    nerlnet::TorchTensor tensor_b = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_BINARY_B], dtype);
    nerlnet::TorchTensor result = tensor_a + tensor_b;

    nifpp::TERM encoded;
    nerlnet::torchbridge::TensorCodec::encode(env, result, encoded);
    return enif_make_tuple2(env, encoded, argv[ARG_BINARY_TYPE]);
}

static ERL_NIF_TERM nerltensor_scalar_multiplication_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_BINARY, ARG_BINARY_TYPE, ARG_SCALAR};
    nifpp::str_atom binary_type;
    nifpp::get_throws(env, argv[ARG_BINARY_TYPE], binary_type);
    c10::ScalarType dtype = nerlnet::get_torch_dtype(binary_type);

    double scalar_value = 0.0;
    if (!enif_get_double(env, argv[ARG_SCALAR], &scalar_value))
    {
        return enif_make_badarg(env);
    }

    nerlnet::TorchTensor tensor = nerlnet::torchbridge::TensorCodec::decode(env, argv[ARG_BINARY], dtype);
    nerlnet::TorchTensor result;
    if (tensor.is_floating_point())
    {
        result = tensor * scalar_value;
    }
    else
    {
        result = (tensor.to(torch::kFloat) * scalar_value).to(dtype);
    }

    nifpp::TERM encoded;
    nerlnet::torchbridge::TensorCodec::encode(env, result, encoded);
    return enif_make_tuple2(env, encoded, argv[ARG_BINARY_TYPE]);
}

static ErlNifFunc nif_funcs[] =
{
    {"get_active_models_ids_list",0, get_active_models_ids_list_nif},
    {"encode_nif", 2, encode_nif},
    {"decode_nif", 2, decode_nif},
    {"nerltensor_sum_nif", 3, nerltensor_sum_nif},
    {"nerltensor_scalar_multiplication_nif", 3, nerltensor_scalar_multiplication_nif},
    {"train_nif", 3 , train_nif},
    {"train_microbatch_nif", 4, train_microbatch_nif},
    {"optimizer_barrier_nif", 1, optimizer_barrier_nif},
    {"pipeline_stage0_forward_nif", 5, pipeline_stage0_forward_nif},
    {"pipeline_stage_forward_nif", 7, pipeline_stage_forward_nif},
    {"pipeline_stage_last_forward_backward_nif", 7, pipeline_stage_last_forward_backward_nif},
    {"pipeline_stage_backward_nif", 5, pipeline_stage_backward_nif},
    {"pipeline_predict_stage0_forward_nif", 3, pipeline_predict_stage0_forward_nif},
    {"pipeline_predict_stage_forward_nif", 3, pipeline_predict_stage_forward_nif},
    {"predict_nif", 3 , predict_nif},
    {"new_nerlworker_nif", 4, nerlnet::torchbridge::new_nerlworker_nif},
    {"test_nerlworker_nif", 4, nerlnet::torchbridge::test_nerlworker_nif},
    {"update_nerlworker_train_params_nif", 6, nerlnet::torchbridge::update_nerlworker_train_params_nif},
    {"remove_nerlworker_nif", 1, nerlnet::torchbridge::remove_nerlworker_nif}
};
