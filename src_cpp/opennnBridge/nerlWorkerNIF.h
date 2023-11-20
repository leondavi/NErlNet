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
    LogInfo << "test_worker_nif" << std::endl;
    
    std::string layer_sizes_str = "16x16k5s2,8,4,3,2";
    std::string layer_types_list = "3,3,3,3,3";
    std::string layers_functionality = "4,4,4,4,4";

    NerlWorkerOpenNN new_worker = NerlWorkerOpenNN(1, layer_sizes_str, layer_types_list, layers_functionality, (float)0.001, 100, 1, 1);
    
    nifpp::str_atom ret_atom = "ok";

    return nifpp::make(env, ret_atom);
}

static ERL_NIF_TERM remove_worker_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifpp::str_atom ret_atom = "ok";

    return nifpp::make(env, ret_atom);

}