
#pragma once

#include "common_definitions.h"
#include "utilities.h"
#include "nerltensor.h"

static bool log_endian_issue_once = true;

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


    bool big_endian = nerlnet_utilities::is_big_endian();
    if (big_endian)
    {
        if(log_endian_issue_once)
        {
            LogError("big endian system! - make sure no little endian in the system!");
            log_endian_issue_once = false;
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