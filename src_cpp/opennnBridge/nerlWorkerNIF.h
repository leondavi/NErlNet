#pragma once

#include <Logger.h>
#include <memory>
#include "nifppNerltensorEigen.h"
#include "nerlWorkerOpenNN.h"
#include "nerlWorkerFunc.h"


using namespace nerlnet;

static std::shared_ptr<NerlWorkerOpenNN> create_nerlworker(std::string &model_type_str,std::string &learning_rate_str,
 std::string &epochs_str, std::string &optimizer_type_str, std::string &loss_method_str,
 std::string &distributed_system_type_str, std::string &layer_sizes_str, std:: string &layer_types_str,
 std::string &layers_functionality_str, std::string &optimizer_args_str, std::string &distributed_system_args_str) //all should be const reference
{
    std::shared_ptr<NerlWorkerOpenNN> new_worker = parse_model_params<NerlWorkerOpenNN>(model_type_str,learning_rate_str,epochs_str,optimizer_type_str,loss_method_str,distributed_system_type_str,layer_sizes_str,
    layer_types_str,layers_functionality_str,optimizer_args_str,distributed_system_args_str);
    return new_worker;
}
static ERL_NIF_TERM new_nerlworker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum{ARG_MODEL_ID,ARG_MODEL_TYPE, ARG_LAYERS_SIZES, ARG_LAYERS_TYPES, ARG_LAYERS_FUNCTIONALITY_CODES, ARG_LEARNING_RATE, ARG_EPOCHS, ARG_OPTIMIZER_TYPE,
         ARG_OPTIMIZER_ARGS, ARG_LOSS_METHOD, ARG_DISTRIBUTED_SYSTEM_TYPE, ARG_DISTRIBUTED_SYSTEM_ARGS};
  
    unsigned long modelId;
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

    nifpp::get_throws(env,argv[ARG_MODEL_ID],modelId);
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

    std::shared_ptr<NerlWorkerOpenNN> new_nerl_worker_ptr =  create_nerlworker(model_type_str,learning_rate_str,epochs_str,optimizer_type_str,loss_method_str,distributed_system_type_str,layer_sizes_str,
    layer_types_str,layers_functionality_str,optimizer_args_str,distributed_system_args_str);
    // Create the singleton instance
    BridgeController& onnBrCtrl = BridgeController::GetInstance();
    // Put the model record to the map with modelId
    onnBrCtrl.setData(new_nerl_worker_ptr, modelId);
    nifpp::str_atom ret_atom = "ok";

    return nifpp::make(env, ret_atom);
}

static ERL_NIF_TERM test_nerlworker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum{ARG_MODEL_ID,ARG_MODEL_TYPE, ARG_LAYERS_SIZES, ARG_LAYERS_TYPES, ARG_LAYERS_FUNCTIONALITY_CODES, ARG_LEARNING_RATE, ARG_EPOCHS, ARG_OPTIMIZER_TYPE,
         ARG_OPTIMIZER_ARGS, ARG_LOSS_METHOD, ARG_DISTRIBUTED_SYSTEM_TYPE, ARG_DISTRIBUTED_SYSTEM_ARGS};
   
    unsigned long modelId;
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
/*  std::vector<int> stam = {1,2,3,4,5};
    std::shared_ptr<intTensor1D> stam_tensor_int;
    std::shared_ptr<iTensor1D> stam_tensor_index;
    vector_to_tensor_1d(stam, stam_tensor_int); //example of how to convert vector to tensor
    convert_tensor_int_to_tensor_index(stam_tensor_int, stam_tensor_index); //example of how to convert tensor int to tensor index
    ///For CNN and multi dimensional tensors
    std::cout<<"stam tensor index:"<<std::endl;
    std::cout<<*stam_tensor_index<<std::endl;
    */

    nifpp::get_throws(env,argv[ARG_MODEL_ID],modelId);
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
    std::shared_ptr<NerlWorkerOpenNN> new_nerl_worker_ptr = create_nerlworker(model_type_str,learning_rate_str,epochs_str,optimizer_type_str,loss_method_str,distributed_system_type_str,layer_sizes_str,
    layer_types_str,layers_functionality_str,optimizer_args_str,distributed_system_args_str);
   std::cout<<"test_nerlworker_nif after"<<std::endl;
     // Create the singleton instance
    BridgeController& onnBrCtrl = BridgeController::GetInstance();
    // Put the model record to the map with modelId
    onnBrCtrl.setData(new_nerl_worker_ptr, modelId);
    nifpp::str_atom ret_atom = "ok";

    return nifpp::make(env, ret_atom);
}

static ERL_NIF_TERM update_nerlworker_train_params_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
      enum{_ModelID,_LearningRate,_Epochs,_OptimizerType,_OptimizerArgs,_LossMethod}; //DO ALL IN CAPS
      nifpp::str_atom ret_atom = "ok";
      return nifpp::make(env, ret_atom);
}


static ERL_NIF_TERM remove_nerlworker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum{ARG_MODEL_ID};
    unsigned long modelId;

    nifpp::get_throws(env,argv[ARG_MODEL_ID],modelId);
    BridgeController& onnBrCtrl = BridgeController::GetInstance();
    onnBrCtrl.deleteModel(modelId);
    LogInfo << "remove_worker_nif" << endl;

    nifpp::str_atom ret_atom = "ok";
    return nifpp::make(env, ret_atom);

}                          