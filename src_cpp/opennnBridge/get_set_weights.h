#pragma once 


#include <string>
#include "ModelParams.h"
#include "nifppNerltensorEigen.h"

using namespace opennn;



static ERL_NIF_TERM get_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ 
    std::tuple<nifpp::TERM, nifpp::TERM> return_tuple;

    nifpp::str_atom res;
    long int mid;
    ErlNifPid pid;
    nifpp::TERM nerltensor_parameters_bin;
    fTensor1D parameters;
    fTensor1DPtr parameters_ptr;
    nifpp::str_atom nerltensor_type = "float";
    std::tuple<nifpp::TERM, nifpp::TERM> message_tuple; // returned nerltensor of parameters

    enif_self(env, &pid);

    //get model id
    nifpp::get_throws(env, argv[0], mid); 

    //get neural network parameters which are weights and biases valuse as a 1D vector         
    opennnBridgeController &onn_bridge_control = opennnBridgeController::GetInstance();
    std::shared_ptr<opennn::NeuralNetwork> neural_network = onn_bridge_control.getModelPtr(mid);

    parameters = neural_network->get_parameters();
    parameters_ptr = std::make_shared<fTensor1D>(parameters);

    nifpp::make_tensor_1d<float, fTensor1D>(env, nerltensor_parameters_bin, parameters_ptr); //binary tensor
    
    // create returned tuple
    message_tuple = { nerltensor_parameters_bin , nifpp::make(env, nerltensor_type) };
    nifpp::TERM message = nifpp::make(env, message_tuple);

    if(enif_send(NULL,&(pid), env, message)) //TODO check this value in Erlang!
    {
        res = "ok";
        return nifpp::make(env, res);
    }
  
    LogError("enif_send failed\n");
    res = "nif_error";
    return nifpp::make(env, res);

}  //end get_weights_nif 



static ERL_NIF_TERM set_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
    // return enif_make_string(env, "end set_weights_nif ", ERL_NIF_LATIN1);
    enum{ARG_ModelID, ARG_Weights, ARG_Type};
    
    long int mid;
    ErlNifPid pid;
    
    fTensor1DPtr parameters;
    
    enif_self(env, &pid);
    opennnBridgeController& s = opennnBridgeController::GetInstance();

    //get model id
    nifpp::get_throws(env, argv[ARG_ModelID], mid); 
    //nifpp::get_throws(env, argv[ARG_Type], mid); // TODO: Add this
    nifpp::get_tensor_1d<float,fTensor1DPtr, fTensor1D>(env,argv[ARG_Weights], parameters);

    //get neural network from singelton           
    std::shared_ptr<opennn::NeuralNetwork> neural_network = s.getModelPtr(mid);
    int nn_parameters_number = neural_network->get_parameters_number();
    int new_parameters_number = parameters->size();
   
    assert(nn_parameters_number == new_parameters_number);
    neural_network->set_parameters(*parameters);

    nifpp::str_atom ret_val("ok");
    return nifpp::make(env, ret_val);
}  //end set_weights_nif 