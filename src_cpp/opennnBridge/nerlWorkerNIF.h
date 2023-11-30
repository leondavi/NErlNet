#pragma once

#include <Logger.h>
#include "nifppNerltensorEigen.h"
#include "nerlWorker.h"

using namespace nerlnet;

static ERL_NIF_TERM new_worker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifpp::str_atom ret_atom = "ok";

    return nifpp::make(env, ret_atom);
}

static ERL_NIF_TERM test_worker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum{ARG_MODEL_TYPE, ARG_LAYERS_SIZES, ARG_LAYERS_TYPES, ARG_LAYERS_FUNCTIONALITY_CODES, ARG_LEARNING_RATE, ARG_EPOCHS, ARG_OPTIMIZER_TYPE,
         ARG_OPTIMIZER_ARGS, ARG_LOSS_METHOD, ARG_DISTRIBUTED_SYSTEM_TYPE, ARG_DISTRIBUTED_SYSTEM_ARGS};
  
    std::string model_type_str;
    std::string layer_sizes_str;
    std::string layer_types_str;
    std::string layers_functionality_str;
    std::string learning_rate_str;
    std::string epochs_str;
    std::string optimizer_type_str;
    std::string optimizer_args_str;
    std::string loss_method_str;
    std::string distributed_system_type_str;
    std::string distributed_system_args_str;

    nifpp::get_throws(env, argv[ARG_MODEL_TYPE], model_type_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_SIZES], layer_sizes_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_TYPES], layer_types_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_FUNCTIONALITY_CODES], layers_functionality_str);
    nifpp::get_throws(env, argv[ARG_LEARNING_RATE], learning_rate_str);
    nifpp::get_throws(env, argv[ARG_EPOCHS], epochs_str);
    nifpp::get_throws(env, argv[ARG_OPTIMIZER_TYPE], optimizer_type_str);
    nifpp::get_throws(env, argv[ARG_OPTIMIZER_ARGS], optimizer_args_str);
    nifpp::get_throws(env, argv[ARG_LOSS_METHOD], loss_method_str);
    nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_TYPE], distributed_system_type_str);
    nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_ARGS], distributed_system_args_str);

    int model_type = std::stoi(model_type_str);
    float learning_rate = std::stof(learning_rate_str);
    int epochs = std::stoi(epochs_str);
    int optimizer_type = std::stoi(optimizer_type_str);
    int loss_method = std::stoi(loss_method_str);
    int distributed_system_type = std::stoi(distributed_system_type_str);

    LogInfo << "test_worker_nif" << std::endl;
    LogInfo << "model_type: " << model_type << std::endl;
    LogInfo << "layer_sizes_str: " << layer_sizes_str << std::endl;
    LogInfo << "layer_types_str: " << layer_types_str << std::endl;
    LogInfo << "layers_functionality_str: " << layers_functionality_str << std::endl;
    LogInfo << "learning_rate: " << learning_rate << std::endl;
    LogInfo << "epochs: " << epochs << std::endl;
    LogInfo << "optimizer_type: " << optimizer_type << std::endl;
    LogInfo << "optimizer_args_str: " << optimizer_args_str << std::endl;
    LogInfo << "loss_method: " << loss_method << std::endl;
    LogInfo << "distributed_system_type: " << distributed_system_type << std::endl;
    LogInfo << "distributed_system_args_str: " << distributed_system_args_str << std::endl;
    

    NerlWorkerOpenNN new_worker = NerlWorkerOpenNN(model_type, layer_sizes_str, layer_types_str, layers_functionality_str,
                                                   learning_rate, epochs, optimizer_type, optimizer_args_str, loss_method, distributed_system_type, distributed_system_args_str );
    
    nifpp::str_atom ret_atom = "ok";

    return nifpp::make(env, ret_atom);
}

static ERL_NIF_TERM remove_worker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifpp::str_atom ret_atom = "ok";

    return nifpp::make(env, ret_atom);

}