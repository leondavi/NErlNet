#pragma once

#include "NerlWorkerTorch.h"
#include "bridgeController.h"

#include <chrono>

class dirty_thread_args
{
public:
    long int mid; // model id
    std::shared_ptr<nerlnet::TorchTensor> data;
    nifpp::str_atom return_tensor_type; // holds the type of tensor should be returned
    std::chrono::high_resolution_clock::time_point start_time;


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

    enum{ARG_MODEL_ID, ARG_NERLTENSOR, ARG_NERLTENSOR_TYPE};

    nifpp::get_throws(env, argv[ARG_NERLTENSOR_TYPE],tensor_type);
    thread_args_ptr->return_tensor_type = tensor_type;
    // extract model id
    nifpp::get_throws(env, argv[ARG_MODEL_ID], thread_args_ptr->mid);
    torch_dtype = nerlnet::get_torch_dtype(tensor_type);
    // extract nerltensor
    nifpp::get_nerltensor<float>(env, argv[ARG_NERLTENSOR], *(thread_args_ptr->data), torch_dtype);

    char* thread_name = "train_thread";
    int thread_create_status = enif_thread_create(thread_name, &(thread_args_ptr->tid), train_threaded_function, (void*) p_thread_args_ptr, NULL);
    void** exit_code;
    if (thread_create_status != 0)
    {
        LogError("failed to call enif_thread_create with train_nif");
        nifpp::str_atom ret_status("train_nif_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        thread_create_status = enif_thread_join(thread_args_ptr->tid, exit_code );
        if (thread_create_status != 0)
        {
            LogError("failed to join with train_nif");
            nifpp::str_atom ret_status("train_nif_error");
            return nifpp::make(env, ret_status);
        }
    }
    return nifpp::make(env, "ok");
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
    // extract nerltensor
    nifpp::get_nerltensor<float>(env, argv[ARG_NERLTENSOR], *(thread_args_ptr->data), torch_dtype);

    char* thread_name = "predict_thread";
    int thread_create_status = enif_thread_create(thread_name, &(thread_args_ptr->tid), predict_threaded_function, (void*) p_thread_args_ptr, NULL);
    void** exit_code;
    if (thread_create_status != 0)
    {
        LogError("failed to call enif_thread_create with predict_nif");
        nifpp::str_atom ret_status("predict_nif_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        thread_create_status = enif_thread_join(thread_args_ptr->tid, exit_code );
        if (thread_create_status != 0)
        {
            LogError("failed to join with predict_nif");
            nifpp::str_atom ret_status("predict_nif_error");
            return nifpp::make(env, ret_status);
        }
    }
    return nifpp::make(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"get_active_models_ids_list",0, get_active_models_ids_list_nif},
    {"train_nif", 3 , train_nif},
    {"predict_nif", 3 , predict_nif},
    // {"get_weights_nif",1, get_weights_nif},
    // {"set_weights_nif",3, set_weights_nif},
    // {"encode_nif",2, encode_nif},
    // {"decode_nif",2, decode_nif},
    // {"nerltensor_sum_nif",3, nerltensor_sum_nif},
    // {"nerltensor_scalar_multiplication_nif",3,nerltensor_scalar_multiplication_nif},
    // // nerlworker functions
    // {"new_nerlworker_nif", 13, new_nerlworker_nif},
    // {"test_nerlworker_nif", 13, test_nerlworker_nif},
    // {"update_nerlworker_train_params_nif", 6, update_nerlworker_train_params_nif},
    // {"remove_nerlworker_nif", 1, remove_nerlworker_nif},
    // {"get_distributed_system_train_labels_count_nif", 1, get_distributed_system_train_labels_count_nif}
};
