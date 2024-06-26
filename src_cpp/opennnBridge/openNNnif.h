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
#include <climits>

#include <Logger.h>
#include "../opennn/opennn/opennn.h"
#include "bridgeController.h"
#include "get_set_weights.h"

#include "nifppNerltensorEigen.h"
#include "nerlWorkerNIF.h"

#define DEBUG_CREATE_NIF 0

#define TRAINING_STRATEGY_SET_DISPLAY_ON   1
#define TRAINING_STRATEGY_SET_DISPLAY_OFF  0

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


inline bool is_big_endian(void)
{
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01020304};

    return bint.c[0] == 1;
}

template <typename T>
T swap_endian(T u)
{
    static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");

    union
    {
        T u;
        unsigned char u8[sizeof(T)];
    } source, dest;

    source.u = u;

    for (size_t k = 0; k < sizeof(T); k++)
        dest.u8[k] = source.u8[sizeof(T) - k - 1];

    return dest.u;
}

static bool log_once = true;
/** 
*  Input: List and the type of the encoded binary (atom from the group ?BINARY_GROUP_NERLTENSOR_TYPE)
*  Output: {Binary,BinaryType}
*  Warning - if _XYZ_LIST_FORM type is double it can be cast to integer if binaryType is an integer
**/
static ERL_NIF_TERM encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 

    enum {ARG_IN_LIST, ARG_IN_TYPE};
    nifpp::str_atom enc_atom_type;
    nifpp::get_throws(env, argv[ARG_IN_TYPE], enc_atom_type);
    std::tuple<nifpp::TERM, nifpp::TERM> return_val;


    bool big_endian = is_big_endian();
    if (big_endian)
    {
        if(log_once)
        {
            LogError("big endian system! - make sure no little endian in the system!");
            log_once = false;
        }
    }
    else
    {
        // Little Endian
    }

    int enc_type_num = atom_str_to_enum(enc_atom_type);

    switch (enc_type_num)
    {
        case ATOM_FLOAT:
        {
            //ineffient implementation
            std::vector<double> in_list;
            std::vector<float> flist;
            nifpp::get(env, argv[ARG_IN_LIST], in_list);
            flist.resize(in_list.size());
            for (int i=0; i<in_list.size(); i++)
            {
                flist[i] = static_cast<float>(in_list[i]);
            }
            size_t binary_size = flist.size() * sizeof(float);
            nifpp::binary bin_term(binary_size);
            unsigned char* in_vec_data_ptr = reinterpret_cast<unsigned char*>(flist.data());
            std::memcpy(bin_term.data, in_vec_data_ptr, binary_size);
            return_val = { nifpp::make(env, bin_term) , nifpp::make(env, enc_atom_type) };
            break;
        }
        case ATOM_DOUBLE:
        {
            std::vector<double> in_list;
            unsigned len;
            enif_get_list_length(env, argv[ARG_IN_LIST], &len);
            nifpp::get_throws(env, argv[ARG_IN_LIST], in_list);

            size_t binary_size = in_list.size() * sizeof(double);
            nifpp::binary bin_term(binary_size);
            unsigned char* in_vec_data_ptr = reinterpret_cast<unsigned char*>(in_list.data());
            std::memcpy(bin_term.data, in_vec_data_ptr, binary_size);
            return_val = { nifpp::make(env, bin_term), nifpp::make(env, enc_atom_type) };
            break;
        }
        case ATOM_INT32:
        {
            std::vector<int> in_list;
            nifpp::get_throws(env,argv[ARG_IN_LIST], in_list);
            size_t binary_size = in_list.size() * sizeof(int);
            nifpp::binary bin_term(binary_size);
            unsigned char* in_vec_data_ptr = reinterpret_cast<unsigned char*>(in_list.data());
            std::memcpy(bin_term.data, in_vec_data_ptr, binary_size);
            return_val = { nifpp::make(env, bin_term ) , nifpp::make(env, enc_atom_type) };
            break;
        }
        case ATOM_INT16:
        {
            std::vector<int> in_list;
            std::vector<int16_t> ilist;
            nifpp::get_throws(env,argv[ARG_IN_LIST], in_list);
            ilist.resize(in_list.size());
            for (int i=0; i<ilist.size(); i++)
            {
                ilist[i] = static_cast<short>(in_list[i]);
            }
            size_t binary_size = in_list.size() * sizeof(int16_t);
            nifpp::binary bin_term(binary_size);
            unsigned char* in_vec_data_ptr = reinterpret_cast<unsigned char*>(ilist.data());
            std::memcpy(bin_term.data, in_vec_data_ptr, binary_size);
            return_val = { nifpp::make(env, bin_term) , nifpp::make(env, enc_atom_type) };
            break;
        }
    }
    return nifpp::make(env, return_val); // make tuple
}  

// decode nerlTensor to EigenTensor --> efficient with DMA copies 
// decode string to eigen - only within cpp 
// get --> create std string from erlang 
// from string to std::vector with vector initialization 
// eigen Map from vetor to eigen Tensor

// decode: nerltensor_str --> eigentensor
//nerltensor_str: string (list of bytes) that represents the nerlTensor given a cpp type (float32, int32, double)

// Input: List, BinaryType  (atom from the group ?BINARY_GROUP_NERLTENSOR_TYPE)
// Output: {List, ListType} (ListType is an atom from the group ?LIST_GROUP_NERLTENSOR_TYPE)
static ERL_NIF_TERM decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
    enum {ARG_BINARY, ARG_TYPE , ARG_LIST = 0};

    std::tuple<nifpp::TERM, nifpp::TERM> return_val;

    nifpp::str_atom type_nerltensor;
    nifpp::str_atom erl_float("erl_float");
    nifpp::str_atom erl_int("erl_int");

    nifpp::get_throws(env, argv[ARG_TYPE], type_nerltensor);
    std::vector<char> bin_vec;
    nifpp::get_binary(env, argv[ARG_BINARY], bin_vec);

    int enc_type_num = atom_str_to_enum(type_nerltensor);

    switch (enc_type_num)
    {
        case ATOM_FLOAT:
        {
            std::vector<float> vec;
            vec.resize(bin_vec.size()/sizeof(float));
            std::memcpy(vec.data(), bin_vec.data(), bin_vec.size());
            return_val = { nifpp::make(env, vec) , nifpp::make(env, erl_float) };
            break;
        }
        case ATOM_DOUBLE:
        {
            std::vector<double> vec;
            vec.resize(bin_vec.size()/sizeof(double));
            std::memcpy(vec.data(), bin_vec.data(), bin_vec.size());
            return_val = { nifpp::make(env, vec) , nifpp::make(env, erl_float) };
            break;
        }
        case ATOM_INT32:
        {
            std::vector<int> vec;
            vec.resize(bin_vec.size()/sizeof(int));
            std::memcpy(vec.data(), bin_vec.data(), bin_vec.size());
            return_val = { nifpp::make(env, vec) , nifpp::make(env, erl_int) };
            break;
        }
        case ATOM_INT16:
        {
            std::vector<int16_t> vec;
            vec.resize(bin_vec.size()/sizeof(int16_t));
            std::memcpy(vec.data(), bin_vec.data(), bin_vec.size());
            return_val = { nifpp::make(env, vec) , nifpp::make(env, erl_int) };
            break;
        }
    }
    return nifpp::make(env, return_val);
}

/**
 * Multiply a tensor by scalar
 * Args: nerltensor binary, type, scalar (regular erl_float)
*/
static ERL_NIF_TERM  nerltensor_scalar_multiplication_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::tuple<nifpp::TERM, nifpp::TERM> return_tuple;
    enum {ARG_BINARY, ARG_TYPE, ARG_SCALAR};
    double scalard;
    nifpp::str_atom nerltensors_type;

    nifpp::get_throws(env, argv[ARG_TYPE], nerltensors_type);
    nifpp::get_throws(env, argv[ARG_SCALAR], scalard);
    int enc_type_num = atom_str_to_enum(nerltensors_type);
    int dims;
    nifpp::TERM nerltensor_bin;

    switch (enc_type_num)
    {
        case ATOM_FLOAT:
        {
            std::shared_ptr<fTensor2D> eigen_tensor;
            dims = nifpp::get_tensor_2d<float,fTensor2DPtr, fTensor2D>(env, argv[ARG_BINARY], eigen_tensor); // TODO - upgrade to 3d
            fTensor2DPtr eigen_tensor_res = make_shared<fTensor2D>(eigen_tensor->dimension(0), eigen_tensor->dimension(1));
            float scalarf = static_cast<float>(scalard);
            (*eigen_tensor_res) = (*eigen_tensor) * scalarf;

            nifpp::make_tensor_2d<float, fTensor2D>(env, nerltensor_bin, eigen_tensor_res);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_DOUBLE:
        {
            std::shared_ptr<dTensor2D> eigen_tensor;
            dims = nifpp::get_tensor_2d<double,dTensor2DPtr, dTensor2D>(env, argv[ARG_BINARY], eigen_tensor); // TODO - upgrade to 3d
                        
            dTensor2DPtr eigen_tensor_res = make_shared<dTensor2D>(eigen_tensor->dimension(0), eigen_tensor->dimension(1));
            (*eigen_tensor_res) = (*eigen_tensor) * scalard;

            nifpp::make_tensor_2d<double, dTensor2D>(env, nerltensor_bin, eigen_tensor_res);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_INT32:
        {
            throw("unsuported type");
            break;
        }
        case ATOM_INT16:
        {
            throw("unsuported type");
            break;
        }
    }
    return nifpp::make(env, return_tuple);
}


template<typename EigenTypePtr> inline void sum_eigen(EigenTypePtr A, EigenTypePtr B, EigenTypePtr &C)
{
    (*C) = (*A) + (*B);
}


static ERL_NIF_TERM nerltensor_sum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::tuple<nifpp::TERM, nifpp::TERM> return_tuple;

    enum {ARG_BINARY_A, ARG_BINARY_B, ARG_TYPE};
    enum {TUPLE_NERLTENSOR_DATA, TUPLE_NERLTENSOR_ATOM_TYPE};

    nifpp::str_atom nerltensors_type;
    nifpp::get_throws(env, argv[ARG_TYPE], nerltensors_type);
    
    int dims;
    int enc_type_num = atom_str_to_enum(nerltensors_type);

    nifpp::TERM nerltensor_bin;

    switch (enc_type_num)
    {
        case ATOM_FLOAT:
        {
             std::shared_ptr<fTensor2D> eigen_tensor_a;

            dims = nifpp::get_tensor_2d<float,fTensor2DPtr, fTensor2D>(env, argv[ARG_BINARY_A], eigen_tensor_a); // TODO - try use the 3d
            fTensor2DPtr eigen_tensor_b;
            nifpp::get_tensor_2d<float,fTensor2DPtr, fTensor2D>(env, argv[ARG_BINARY_B], eigen_tensor_b);
            
            fTensor2DPtr eigen_tensor_c = make_shared<fTensor2D>(eigen_tensor_a->dimension(0), eigen_tensor_a->dimension(1));
            sum_eigen<fTensor2DPtr>(eigen_tensor_a, eigen_tensor_b, eigen_tensor_c);
            nifpp::make_tensor_2d<float, fTensor2D>(env, nerltensor_bin, eigen_tensor_c);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_DOUBLE:
        {
            dTensor2DPtr eigen_tensor_a;
            dims = nifpp::get_tensor_2d<double,dTensor2DPtr,dTensor2D>(env, argv[ARG_BINARY_A], eigen_tensor_a); //TODO try use the 3d 
            dTensor2DPtr eigen_tensor_b;
            nifpp::get_tensor_2d<double,dTensor2DPtr,dTensor2D>(env, argv[ARG_BINARY_B], eigen_tensor_b);
            
            dTensor2DPtr eigen_tensor_c = make_shared<dTensor2D>(eigen_tensor_a->dimension(0), eigen_tensor_a->dimension(1));
            sum_eigen<dTensor2DPtr>(eigen_tensor_a, eigen_tensor_b, eigen_tensor_c);
            
            nifpp::make_tensor_2d<double, dTensor2D>(env, nerltensor_bin, eigen_tensor_c);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_INT32:
        {
            throw("unsuported type");
            break;
        }
        case ATOM_INT16:
        {
            throw("unsuported type");
            break;
        }

    }

    return nifpp::make(env, return_tuple);
}


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
    {"new_nerlworker_nif", 13, new_nerlworker_nif},
    {"test_nerlworker_nif", 13, test_nerlworker_nif},
    {"update_nerlworker_train_params_nif", 6, update_nerlworker_train_params_nif},
    {"remove_nerlworker_nif", 1, remove_nerlworker_nif}
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
