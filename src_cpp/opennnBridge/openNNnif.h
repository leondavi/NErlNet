/*****************************************************
 * Authors: David Leon, Ziv Moldavsky, Evgeny Andrachnik
 * 22/7/2022
 * 
 * Copyright (c) 2021 Nerlnet
 *****************************************************/ 

#pragma once 


#include <vector>
#include <string>
#include <map>
#include <chrono>
#include <cstdint>

#include <Logger.h>
#include "../opennn/opennn/opennn.h"
#include "bridgeController.h"
#include "get_set_weights.h"

#include "nerlWorkerNIF.h"

#include "encode_nerltensor_nif.h"
#include "decode_nerltensor_nif.h"
#include "operations_nerltensor_nif.h"

using namespace std;
using namespace chrono;
using namespace opennn;        

class TrainNN
{
public:
    long int mid;

    fTensor2DPtr data;
    std::chrono::high_resolution_clock::time_point start_time;
    double K_val;
    nifpp::str_atom return_tensor_type; // holds the type of tensor should be returned

    ErlNifTid tid;
    ErlNifPid pid;
};

class PredictNN 
{
public:
    long int mid;
    fTensor2DPtr data;
    ErlNifPid pid;
    ErlNifTid tid;

    std::chrono::high_resolution_clock::time_point start_time;

    nifpp::str_atom return_tensor_type; // holds the type of tensor should be returned
};

void* PredictFun(void* arg);

static ERL_NIF_TERM predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 

    std::shared_ptr<PredictNN>* pPredictNNptr = new std::shared_ptr<PredictNN>(std::make_shared<PredictNN>());
    std::shared_ptr<PredictNN> PredictNNptr = *pPredictNNptr;

    PredictNNptr->start_time = high_resolution_clock::now();

    enum{ARG_ModelID, ARG_BatchTensor, ARG_Type};

    ErlNifPid pid;
    enif_self(env, &pid);
    PredictNNptr->pid = pid;

    nifpp::str_atom tensor_type;
    nifpp::get_throws(env, argv[ARG_Type],tensor_type);
    assert(tensor_type == "float");
    PredictNNptr->return_tensor_type = tensor_type;

    nifpp::get_throws(env, argv[ARG_ModelID], PredictNNptr->mid); // get model id
    //std::cout << "PredictNNptr->mid: " << PredictNNptr->mid << std::endl;
    //std::cout << "argv[ARG_BatchTensor]: " << argv[ARG_BatchTensor] << std::endl;
    nifpp::get_tensor_2d<float,fTensor2DPtr,fTensor2D>(env,argv[ARG_BatchTensor],PredictNNptr->data);

    int res;
    void** exit_code;
    res = enif_thread_create((char*)"predict_nif_proc", &(PredictNNptr->tid), PredictFun, (void*) pPredictNNptr, 0);
    if (res)
    {
        LogError("failed to call enif_thread_create with PredictFun");
        nifpp::str_atom ret_status("predict_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        res = enif_thread_join(PredictNNptr->tid, exit_code );
        if (res)
        {
            LogError("failed to join with PredictFun");
            nifpp::str_atom ret_status("predict_error");
            return nifpp::make(env, ret_status);
        }
    }

    nifpp::str_atom ret_status("ok");
    return nifpp::make(env, ret_status);

}  //end PREDICT mode

void* trainFun(void* arg);

static ERL_NIF_TERM train_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    std::shared_ptr<TrainNN>* pTrainNNptr = new std::shared_ptr<TrainNN>(std::make_shared<TrainNN>());
    std::shared_ptr<TrainNN> TrainNNptr = *pTrainNNptr;
    TrainNNptr->start_time = high_resolution_clock::now();
    
    nifpp::str_atom tensor_type;
    enum{ARG_ModelID,ARG_DataTensor,ARG_Type};
    nifpp::get_throws(env, argv[ARG_ModelID],TrainNNptr->mid); // model id
    nifpp::get_throws(env, argv[ARG_Type],tensor_type);
    assert(tensor_type == "float");
    TrainNNptr->return_tensor_type = tensor_type;
    nifpp::get_tensor_2d<float,fTensor2DPtr,fTensor2D>(env,argv[ARG_DataTensor],TrainNNptr->data);
    ErlNifPid pid;
    enif_self(env, &pid);
    TrainNNptr->pid = pid;
    int res;

    void** exit_code;
    res = enif_thread_create((char*)"train_nif_proc", &(TrainNNptr->tid), trainFun, (void*) pTrainNNptr, 0);
    if (res)
    {
        LogError("failed to call enif_thread_create with trainFun");
        nifpp::str_atom ret_status("train_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        res = enif_thread_join(TrainNNptr->tid, exit_code );
        if (res)
        {
            LogError("failed to join with trainFun");
            nifpp::str_atom ret_status("train_error");
            return nifpp::make(env, ret_status);
        }
    }

    nifpp::str_atom ret_status("ok");
    return nifpp::make(env, ret_status);
}  //end trainn_nif




static ErlNifFunc nif_funcs[] =
{
    {"get_active_models_ids_list",0, get_active_models_ids_list_nif},
    {"train_nif", 3 , train_nif},
    {"predict_nif", 3 , predict_nif},
    {"get_weights_nif",1, get_weights_nif},
    {"set_weights_nif",3, set_weights_nif},
    {"encode_nif",2, encode_nif},
    {"decode_nif",2, decode_nif},
    {"nerltensor_sum_nif",3, nerltensor_sum_nif},
    {"nerltensor_scalar_multiplication_nif",3,nerltensor_scalar_multiplication_nif},
    // nerlworker functions
    {"new_nerlworker_nif", 14, new_nerlworker_nif},
    {"test_nerlworker_nif", 14, test_nerlworker_nif},
    {"update_nerlworker_train_params_nif", 6, update_nerlworker_train_params_nif},
    {"remove_nerlworker_nif", 1, remove_nerlworker_nif},
    {"get_distributed_system_train_labels_count_nif", 1, get_distributed_system_train_labels_count_nif}
};


// load_info is the second argument to erlang:load_nif/2.
// *priv_data can be set to point to some private data if the library needs to keep a state between NIF calls.
// enif_priv_data returns this pointer. *priv_data is initialized to NULL when load is called.
// The library fails to load if load returns anything other than 0. load can be NULL if initialization is not needed.
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    //nifpp::register_resource<GetcppBridgeController>(env, nullptr, "GetcppBridgeController");
    //nifpp::register_resource<cppBridgeController>(env, nullptr, "cppBridgeController");
   // nifpp::register_resource<SANN::Model>(env, nullptr, "cppBridgeController");
    return 0;
}

// This is the magic macro to initialize a NIF library. It is to be evaluated in global file scope.
// ERL_NIF_INIT(MODULE, ErlNifFunc funcs[], load, NULL, upgrade, unload)
// MODULE -  The first argument must be the name of the Erlang module as a C-identifier. It will be stringified by the macro.
// ErlNifFunc - The second argument is the array of ErlNifFunc structures containing name, arity, and function pointer of each NIF.
// load -  is called when the NIF library is loaded and no previously loaded library exists for this module.
// NULL - The fourth argument NULL is ignored. It was earlier used for the deprecated reload callback which is no longer supported since OTP 20.
// The remaining arguments are pointers to callback functions that can be used to initialize the library.
// They are not used in this simple example, hence they are all set to NULL.
ERL_NIF_INIT(nerlNIF, nif_funcs, load, NULL, NULL, NULL)

//ERL_NIF_INIT(nerlNIF,nif_funcs,NULL,NULL,NULL,NULL)
