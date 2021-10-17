#pragma once 

//#include <iostream>
#include "nifpp.h"
#include <erl_nif.h>
#include <vector>
#include <string>
#include "ModelParams.h"

#include "Eigen/Core"
#include "unsupported/Eigen/CXX11/Tensor"
#include "opennn.h"

using namespace OpenNN;

#define DEBUG_CREATE_NIF 0

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try{
         ModelParams modelParamsInst;
         //std::vector<int> activation_list1;
         nifpp::get_throws(env,argv[0],*(modelParamsInst.GetAcvtivationList()));
         //nifpp::get_throws(env,argv[1],modelParamPtr.model_type); TODO
        }
    catch(nifpp::badarg){
            return enif_make_badarg(env);
     }
    
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
/*
#if DEBUG_CREATE_NIF
            std::cout << "Optimizers::opt_t optimizer: " << optimizer << '\n';
            std::cout << "act_types_vec: " << act_types_vec << '\n';
            std::cout << "modelParamPtr.layers_sizes: " << modelParamPtr.layers_sizes << '\n';
            std::cout << "modelParamPtr.learning_rate: " << modelParamPtr.learning_rate << '\n';
            std::cout << "Start creating the module." << '\n';
#endif
*/
}

static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello}
};

// TODO: Think about using this feature in the future
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
ERL_NIF_INIT(niftest, nif_funcs, load, NULL, NULL, NULL)

//ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)
