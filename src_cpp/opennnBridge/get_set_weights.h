#pragma once 


#include <string>
#include "../simple-cpp-logger/include/Logger.h"
#include "nifppNerltensorEigen.h"
#include "bridgeController.h"
#include "nerlWorkerOpenNN.h"

#define GET_WEIGHTS_ATOM_STR "get_weights"

using namespace opennn;
using namespace nerlnet;

class GetWeightsParams 
{
public:
    long int mid;
    ErlNifPid pid;
    ErlNifTid tid;
};

inline void* get_weights(void* arg)
{
    std::shared_ptr<GetWeightsParams>* pGetWeigthsParamsPtr= static_cast<shared_ptr<GetWeightsParams>*>(arg);
    std::shared_ptr<GetWeightsParams> getWeigthsParamsPtr = *pGetWeigthsParamsPtr;
    delete pGetWeigthsParamsPtr; // This is owned by the former thread

    ErlNifEnv *env = enif_alloc_env();    
    
    nifpp::TERM nerltensor_parameters_bin;
    fTensor1D parameters;
    fTensor1DPtr parameters_ptr;
    nifpp::str_atom nerltensor_type = "float";
    nifpp::str_atom get_weights_atom = GET_WEIGHTS_ATOM_STR;
    std::tuple<nifpp::TERM, nifpp::TERM, nifpp::TERM> message_tuple; // returned nerltensor of parameters
    
    //get neural network parameters which are weights and biases valuse as a 1D vector         
    BridgeController &bridge_controller = BridgeController::GetInstance();
    std::shared_ptr<NerlWorker> nerlworker = bridge_controller.getModelPtr(getWeigthsParamsPtr->mid);
    std::shared_ptr<NerlWorkerOpenNN> nerlworker_opennn = std::static_pointer_cast<NerlWorkerOpenNN>(nerlworker);
    std::shared_ptr<opennn::NeuralNetwork> neural_network = nerlworker_opennn->get_neural_network_ptr();

    parameters = neural_network->get_parameters();
    parameters_ptr = std::make_shared<fTensor1D>(parameters);

    nifpp::make_tensor_1d<float, fTensor1D>(env, nerltensor_parameters_bin, parameters_ptr); //binary tensor
    
    // create returned tuple
    message_tuple = { nifpp::make(env, get_weights_atom), nerltensor_parameters_bin , nifpp::make(env, nerltensor_type) };
    nifpp::TERM message = nifpp::make(env, message_tuple);

    if(enif_send(NULL,&(getWeigthsParamsPtr->pid), env, message)) //TODO check this value in Erlang!
    {

    }
    else
    {
        LogError("enif_send failed ");
    }

    return 0;
}


static ERL_NIF_TERM get_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::shared_ptr<GetWeightsParams>* pWeightsParamsPtr = new std::shared_ptr<GetWeightsParams>(std::make_shared<GetWeightsParams>());
    std::shared_ptr<GetWeightsParams> weightsParamsPtr = *pWeightsParamsPtr;

    long int mid;
    ErlNifPid pid;

    //get process id
    enif_self(env, &pid);
    weightsParamsPtr->pid = pid;
    //get model id
    nifpp::get_throws(env, argv[0], mid); 
    weightsParamsPtr->mid = mid;

    void** exit_code;
    int res = enif_thread_create((char*)"get_weights_proc", &(weightsParamsPtr->tid), get_weights, (void*) pWeightsParamsPtr, 0);
    if (res)
    {
        LogError("failed to call enif_thread_create with get_weights_proc");
        nifpp::str_atom ret_status("get_weights_error");
        return nifpp::make(env, ret_status);
    }
    else
    {
        res = enif_thread_join(weightsParamsPtr->tid, exit_code );
        if (res)
        {
            LogError("failed to join with get_weights_proc");
            nifpp::str_atom ret_status("get_weights_error");
            return nifpp::make(env, ret_status);
        }
    }
    nifpp::str_atom ret_status("ok");
    return nifpp::make(env, res);

}  //end get_weights_nif 



static ERL_NIF_TERM set_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
    // return enif_make_string(env, "end set_weights_nif ", ERL_NIF_LATIN1);
    enum{ARG_ModelID, ARG_Weights, ARG_Type};
    
    long int mid;
    ErlNifPid pid;
    
    fTensor1DPtr parameters;
    
    enif_self(env, &pid);
    //get model id
    nifpp::get_throws(env, argv[ARG_ModelID], mid); 
    //get nerlworker from bridge controller
    BridgeController &bridge_controller = BridgeController::GetInstance();
    std::shared_ptr<NerlWorker> nerlworker = bridge_controller.getModelPtr(mid);
    std::shared_ptr<NerlWorkerOpenNN> nerlworker_opennn = std::static_pointer_cast<NerlWorkerOpenNN>(nerlworker);
    //get neural network from nerlworker
    std::shared_ptr<opennn::NeuralNetwork> neural_network = nerlworker_opennn->get_neural_network_ptr();

    nifpp::get_tensor_1d<float,fTensor1DPtr, fTensor1D>(env,argv[ARG_Weights], parameters);

    int nn_parameters_number = neural_network->get_parameters_number();
    int new_parameters_number = parameters->size();
   
    assert(nn_parameters_number == new_parameters_number);
    neural_network->set_parameters(*parameters);

    nifpp::str_atom ret_val("ok");
    return nifpp::make(env, ret_val);
}  //end set_weights_nif 