#include "torchNIF.h"

#include "../common/common_definitions.h"

#include <string>

namespace
{
using nerlnet::BridgeController;
using nerlnet::NerlWorker;
using nerlnet::NerlWorkerTorch;
using nerlnet::TorchTensor;
using nerlnet::torchbridge::TensorCodec;

ERL_NIF_TERM make_error(ErlNifEnv *env, const std::string &message)
{
    ERL_NIF_TERM nerlnif_atom = enif_make_atom(env, NERLNIF_ATOM_STR);
    nifpp::TERM error_atom = nifpp::make(env, nifpp::str_atom("error"));
    nifpp::TERM msg_term = nifpp::make(env, message);
    return enif_make_tuple(env, 3, nerlnif_atom, error_atom, msg_term);
}

template <typename Functor>
ERL_NIF_TERM execute_with_worker(std::shared_ptr<dirty_thread_args> &thread_args_ptr, Functor &&functor)
{
    BridgeController &controller = BridgeController::GetInstance();
    std::shared_ptr<NerlWorker> worker_base = controller.getModelPtr(thread_args_ptr->mid);
    std::shared_ptr<NerlWorkerTorch> worker = std::static_pointer_cast<NerlWorkerTorch>(worker_base);
    return functor(worker);
}
}

void* train_threaded_function(void* args)
{
    std::shared_ptr<dirty_thread_args>* p_thread_args_ptr = static_cast<std::shared_ptr<dirty_thread_args>*>(args);
    std::shared_ptr<dirty_thread_args> thread_args_ptr = *p_thread_args_ptr;
    delete p_thread_args_ptr;

    ErlNifEnv *env = enif_alloc_env();
    ERL_NIF_TERM message;

    try
    {
        message = execute_with_worker(thread_args_ptr, [&](const std::shared_ptr<NerlWorkerTorch> &worker) {
            TorchTensor loss = worker->train_batch(thread_args_ptr->nerltensor);
            nifpp::TERM encoded_loss;
            TensorCodec::encode(env, loss, encoded_loss);

            auto stop = std::chrono::high_resolution_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - thread_args_ptr->start_time);

            ERL_NIF_TERM nerlnif_atom = enif_make_atom(env, NERLNIF_ATOM_STR);
            ERL_NIF_TERM train_time = enif_make_double(env, static_cast<double>(duration.count()));

            return enif_make_tuple(env,
                                    4,
                                    nerlnif_atom,
                                    encoded_loss,
                                    nifpp::make(env, thread_args_ptr->return_tensor_type),
                                    train_time);
        });
    }
    catch (const std::exception &ex)
    {
        LogError << "Torch train failed: " << ex.what() << std::endl;
        message = make_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "Torch train failed with unknown error" << std::endl;
        message = make_error(env, "unknown_train_error");
    }

    if (!enif_send(NULL, &(thread_args_ptr->pid), env, message))
    {
        LogError << "enif_send failed for torch train" << std::endl;
    }

    enif_free_env(env);
    return nullptr;
}


void* predict_threaded_function(void* args)
{
    std::shared_ptr<dirty_thread_args>* p_thread_args_ptr = static_cast<std::shared_ptr<dirty_thread_args>*>(args);
    std::shared_ptr<dirty_thread_args> thread_args_ptr = *p_thread_args_ptr;
    delete p_thread_args_ptr;

    ErlNifEnv *env = enif_alloc_env();
    ERL_NIF_TERM message;

    try
    {
        message = execute_with_worker(thread_args_ptr, [&](const std::shared_ptr<NerlWorkerTorch> &worker) {
            TorchTensor prediction = worker->predict_batch(thread_args_ptr->nerltensor);
            if (prediction.scalar_type() != thread_args_ptr->tensor_dtype)
            {
                prediction = prediction.to(thread_args_ptr->tensor_dtype);
            }

            nifpp::TERM encoded_prediction;
            TensorCodec::encode(env, prediction, encoded_prediction);

            auto stop = std::chrono::high_resolution_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - thread_args_ptr->start_time);

            ERL_NIF_TERM nerlnif_atom = enif_make_atom(env, NERLNIF_ATOM_STR);
            ERL_NIF_TERM predict_time = enif_make_double(env, static_cast<double>(duration.count()));

            return enif_make_tuple(env,
                                    4,
                                    nerlnif_atom,
                                    encoded_prediction,
                                    nifpp::make(env, thread_args_ptr->return_tensor_type),
                                    predict_time);
        });
    }
    catch (const std::exception &ex)
    {
        LogError << "Torch predict failed: " << ex.what() << std::endl;
        message = make_error(env, ex.what());
    }
    catch (...)
    {
        LogError << "Torch predict failed with unknown error" << std::endl;
        message = make_error(env, "unknown_predict_error");
    }

    if (!enif_send(NULL, &(thread_args_ptr->pid), env, message))
    {
        LogError << "enif_send failed for torch predict" << std::endl;
    }

    enif_free_env(env);
    return nullptr;
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM)
{
    return 0;
}

ERL_NIF_INIT(nerlTorchNIF, nif_funcs, load, NULL, NULL, NULL)