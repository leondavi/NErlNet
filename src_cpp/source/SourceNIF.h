#pragma once

#include "nifpp.h"
#include "SourceCSV.h"
#include <Logger.h>



static ERL_NIF_TERM set_source_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum {ARG_SOURCE_TYPE, ARG_DATA_TYPE, ARG_BATCH_SIZE, ARG_CUSTOM_PARAMS_STR};
}

static ERL_NIF_TERM source_get_batches_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    
}

static ERL_NIF_TERM source_more_batches_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    
}

static ErlNifFunc nif_funcs[] =
{
    {"set_source_nif",4, set_source_nif},
    {"source_get_batches_nif", 0 , source_get_batches_nif},
    {"source_more_batches_nif", 0 , source_more_batches_nif}
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

ERL_NIF_INIT(sourceNIF, nif_funcs, NULL, NULL, NULL, NULL)

// This is the magic macro to initialize a NIF library. It is to be evaluated in global file scope.
// ERL_NIF_INIT(MODULE, ErlNifFunc funcs[], load, NULL, upgrade, unload)
// MODULE -  The first argument must be the name of the Erlang module as a C-identifier. It will be stringified by the macro.
// ErlNifFunc - The second argument is the array of ErlNifFunc structures containing name, arity, and function pointer of each NIF.
// load -  is called when the NIF library is loaded and no previously loaded library exists for this module.
// NULL - The fourth argument NULL is ignored. It was earlier used for the deprecated reload callback which is no longer supported since OTP 20.
// The remaining arguments are pointers to callback functions that can be used to initialize the library.

//ERL_NIF_INIT(nerlNIF,nif_funcs,NULL,NULL,NULL,NULL)

