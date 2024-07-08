#pragma once


#include "common_definitions.h"

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
    enum {ARG_BINARY, ARG_TYPE};

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
