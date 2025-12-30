#pragma once

#include <Logger.h>
#include <memory>
#include <utility>
#include "nifppNerltensorEigen.h"
#include "nerlWorkerOpenNN.h"
#include "nerlWorkerFunc.h"


using namespace nerlnet;

static std::shared_ptr<NerlWorkerOpenNN> create_nerlworker(std::string &model_type_str, std::string &model_args_str, std::string &learning_rate_str,
 std::string &epochs_str, std::string &optimizer_type_str, std::string &loss_method_str, std::string &loss_args_str,
 std::string &distributed_system_type_str, std::string &layer_sizes_str, std:: string &layer_types_str,
std::string &layers_functionality_str, std::string &optimizer_args_str, std::string &distributed_system_args_str) //all should be const reference
{
    NerlWorker::WorkerParams worker_params = {
        {"model_type", model_type_str},
        {"model_args", model_args_str},
        {"learning_rate", learning_rate_str},
        {"epochs", epochs_str},
        {"optimizer_type", optimizer_type_str},
        {"optimizer_args", optimizer_args_str},
        {"loss_method", loss_method_str},
        {"loss_args", loss_args_str},
        {"layer_sizes", layer_sizes_str},
        {"layer_types", layer_types_str},
        {"layers_functionality", layers_functionality_str}
    };
    int distributed_system_type = std::stoi(distributed_system_type_str);
    return std::make_shared<NerlWorkerOpenNN>(std::move(worker_params),
                                              distributed_system_type,
                                              distributed_system_args_str);
}
static ERL_NIF_TERM new_nerlworker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum{ARG_MODEL_ID,ARG_MODEL_TYPE, ARG_MODEL_ARGS, ARG_LAYERS_SIZES, ARG_LAYERS_TYPES, ARG_LAYERS_FUNCTIONALITY_CODES, ARG_LEARNING_RATE, ARG_EPOCHS, ARG_OPTIMIZER_TYPE,
         ARG_OPTIMIZER_ARGS, ARG_LOSS_METHOD, ARG_LOSS_ARGS, ARG_DISTRIBUTED_SYSTEM_TYPE, ARG_DISTRIBUTED_SYSTEM_ARGS};
  
    unsigned long modelId;
    std::string model_type_str;
    std::string model_args_str;
    std::string layer_sizes_str;
    std::string layer_types_str;
    std::string layers_functionality_str;
    std::string learning_rate_str;
    std::string epochs_str;
    std::string optimizer_type_str;
    std::string optimizer_args_str;
    std::string loss_method_str;
    std::string loss_args_str;
    std::string distributed_system_type_str;
    std::string distributed_system_args_str;

    nifpp::get_throws(env,argv[ARG_MODEL_ID],modelId);
    nifpp::get_throws(env, argv[ARG_MODEL_TYPE], model_type_str);
    nifpp::get_throws(env, argv[ARG_MODEL_ARGS], model_args_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_SIZES], layer_sizes_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_TYPES], layer_types_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_FUNCTIONALITY_CODES], layers_functionality_str);
    nifpp::get_throws(env, argv[ARG_LEARNING_RATE], learning_rate_str);
    nifpp::get_throws(env, argv[ARG_EPOCHS], epochs_str);
    nifpp::get_throws(env, argv[ARG_OPTIMIZER_TYPE], optimizer_type_str);
    nifpp::get_throws(env, argv[ARG_OPTIMIZER_ARGS], optimizer_args_str);
    nifpp::get_throws(env, argv[ARG_LOSS_METHOD], loss_method_str);
    nifpp::get_throws(env, argv[ARG_LOSS_ARGS], loss_args_str);
    nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_TYPE], distributed_system_type_str);
    nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_ARGS], distributed_system_args_str);

    std::shared_ptr<NerlWorkerOpenNN> new_nerl_worker_ptr =  create_nerlworker(model_type_str,model_args_str,learning_rate_str,epochs_str,optimizer_type_str,
                                                                            loss_method_str,loss_args_str,distributed_system_type_str,layer_sizes_str,
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
    enum{ARG_MODEL_ID,ARG_MODEL_TYPE, ARG_MODEL_ARGS , ARG_LAYERS_SIZES, ARG_LAYERS_TYPES, ARG_LAYERS_FUNCTIONALITY_CODES, ARG_LEARNING_RATE, ARG_EPOCHS, ARG_OPTIMIZER_TYPE,
         ARG_OPTIMIZER_ARGS, ARG_LOSS_METHOD, ARG_LOSS_ARGS, ARG_DISTRIBUTED_SYSTEM_TYPE, ARG_DISTRIBUTED_SYSTEM_ARGS};
   
    unsigned long modelId;
    std::string model_type_str;
    std::string model_args_str;
    std::string layer_sizes_str;
    std::string layer_types_str;
    std::string layers_functionality_str;
    std::string learning_rate_str;
    std::string epochs_str;
    std::string optimizer_type_str;
    std::string optimizer_args_str;
    std::string loss_method_str;
    std::string loss_args_str;
    std::string distributed_system_type_str;
    std::string distributed_system_args_str;

    nifpp::get_throws(env,argv[ARG_MODEL_ID],modelId);
    nifpp::get_throws(env, argv[ARG_MODEL_TYPE], model_type_str);
    nifpp::get_throws(env, argv[ARG_MODEL_ARGS], model_args_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_SIZES], layer_sizes_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_TYPES], layer_types_str);
    nifpp::get_throws(env, argv[ARG_LAYERS_FUNCTIONALITY_CODES], layers_functionality_str);
    nifpp::get_throws(env, argv[ARG_LEARNING_RATE], learning_rate_str);
    nifpp::get_throws(env, argv[ARG_EPOCHS], epochs_str);
    nifpp::get_throws(env, argv[ARG_OPTIMIZER_TYPE], optimizer_type_str);
    nifpp::get_throws(env, argv[ARG_OPTIMIZER_ARGS], optimizer_args_str);
    nifpp::get_throws(env, argv[ARG_LOSS_METHOD], loss_method_str);
    nifpp::get_throws(env, argv[ARG_LOSS_ARGS], loss_args_str);
    nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_TYPE], distributed_system_type_str);
    nifpp::get_throws(env, argv[ARG_DISTRIBUTED_SYSTEM_ARGS], distributed_system_args_str);
    std::shared_ptr<NerlWorkerOpenNN> new_nerl_worker_ptr = create_nerlworker(model_type_str,model_args_str,learning_rate_str,epochs_str,
                                                                            optimizer_type_str,loss_method_str,loss_args_str,distributed_system_type_str,layer_sizes_str,
                                                                            layer_types_str,layers_functionality_str,optimizer_args_str,distributed_system_args_str);
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

    nifpp::str_atom ret_atom = "ok";
    return nifpp::make(env, ret_atom);

}

/**  input -  unsigned long modelId 
 *   output - nerltensor that is the acc sum of each label in the last training data_set
 * **/
static ERL_NIF_TERM get_distributed_system_train_labels_count_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum{ARG_MODEL_ID};
    unsigned long modelId;

    nifpp::get_throws(env,argv[ARG_MODEL_ID],modelId);

    BridgeController& bridge_controller = BridgeController::GetInstance();
    std::shared_ptr<NerlWorkerOpenNN> nerl_worker_ptr = std::static_pointer_cast<NerlWorkerOpenNN>(bridge_controller.getModelPtr(modelId));
    // assert: the model is a distributed system of federated weighted average classification
    std::shared_ptr<std::vector<int>> train_labels_count = nerl_worker_ptr->get_distributed_system_train_labels_count();
    nifpp::str_atom nerltensor_type = "erl_float";
    fTensor1DPtr train_labels_count_tesnsor_ptr;
    nifpp::TERM term_label_count;
    std::tuple<nifpp::TERM, nifpp::TERM> nerltensor_tuple; 

    train_labels_count_tesnsor_ptr = std::make_shared<fTensor1D>(train_labels_count->size());
     for(int i = 0; i < train_labels_count->size(); i++)
    {
        float val = (*train_labels_count)[i];
        train_labels_count_tesnsor_ptr->data()[i] = val;
    }
    nifpp::make_tensor_1d<float, fTensor1D>(env, term_label_count, train_labels_count_tesnsor_ptr); //binary tensor
    nerltensor_tuple = { term_label_count , nifpp::make(env, nerltensor_type) };
    // Return tuple of {nerltensor, nerltensor_type}
    return nifpp::make(env, nerltensor_tuple); // returns NerlTensor erl_int
}