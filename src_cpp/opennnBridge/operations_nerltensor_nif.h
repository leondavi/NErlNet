#pragma once

#include "nifppNerltensorEigen.h"


/**
 * Multiply a tensor by scalar
 * Args: nerltensor binary, type, scalar (regular erl_float)
*/
static ERL_NIF_TERM  nerltensor_scalar_multiplication_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::tuple<nifpp::TERM, nifpp::TERM> return_tuple;
    enum {ARG_BINARY, ARG_TYPE, ARG_SCALAR};
    double scalard;
    nifpp::str_atom nerltensors_type;

    nifpp::get_throws(env, argv[ARG_TYPE], nerltensors_type);
    nifpp::get_throws(env, argv[ARG_SCALAR], scalard);
    int enc_type_num = atom_str_to_enum(nerltensors_type);
    int dims; // TODO inspect this variable
    nifpp::TERM nerltensor_bin;

    switch (enc_type_num)
    {
        case ATOM_FLOAT:
        {
            std::shared_ptr<fTensor2D> eigen_tensor;
            dims = nifpp::get_tensor_2d<float,fTensor2DPtr, fTensor2D>(env, argv[ARG_BINARY], eigen_tensor); // TODO - upgrade to 3d
            fTensor2DPtr eigen_tensor_res = make_shared<fTensor2D>(eigen_tensor->dimension(DIM_X_IDX), eigen_tensor->dimension(DIM_Y_IDX));
            float scalarf = static_cast<float>(scalard);
            (*eigen_tensor_res) = (*eigen_tensor) * scalarf;

            nifpp::make_tensor_2d<float, fTensor2D>(env, nerltensor_bin, eigen_tensor_res);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_DOUBLE:
        {
            std::shared_ptr<dTensor2D> eigen_tensor;
            dims = nifpp::get_tensor_2d<double,dTensor2DPtr, dTensor2D>(env, argv[ARG_BINARY], eigen_tensor); // TODO - upgrade to 3d
                        
            dTensor2DPtr eigen_tensor_res = make_shared<dTensor2D>(eigen_tensor->dimension(0), eigen_tensor->dimension(1));
            (*eigen_tensor_res) = (*eigen_tensor) * scalard;

            nifpp::make_tensor_2d<double, dTensor2D>(env, nerltensor_bin, eigen_tensor_res);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_INT32:
        {
            throw("unsuported type");
            break;
        }
        case ATOM_INT16:
        {
            throw("unsuported type");
            break;
        }
    }
    return nifpp::make(env, return_tuple);
}


template<typename EigenTypePtr> inline void sum_eigen(EigenTypePtr A, EigenTypePtr B, EigenTypePtr &C)
{
    (*C) = (*A) + (*B);
}


static ERL_NIF_TERM nerltensor_sum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::tuple<nifpp::TERM, nifpp::TERM> return_tuple;

    enum {ARG_BINARY_A, ARG_BINARY_B, ARG_TYPE};
    enum {TUPLE_NERLTENSOR_DATA, TUPLE_NERLTENSOR_ATOM_TYPE};

    nifpp::str_atom nerltensors_type;
    nifpp::get_throws(env, argv[ARG_TYPE], nerltensors_type);
    
    int dims;
    int enc_type_num = atom_str_to_enum(nerltensors_type);

    nifpp::TERM nerltensor_bin;

    switch (enc_type_num)
    {
        case ATOM_FLOAT:
        {
             std::shared_ptr<fTensor2D> eigen_tensor_a;

            dims = nifpp::get_tensor_2d<float,fTensor2DPtr, fTensor2D>(env, argv[ARG_BINARY_A], eigen_tensor_a); // TODO - try use the 3d
            fTensor2DPtr eigen_tensor_b;
            nifpp::get_tensor_2d<float,fTensor2DPtr, fTensor2D>(env, argv[ARG_BINARY_B], eigen_tensor_b);
            
            fTensor2DPtr eigen_tensor_c = make_shared<fTensor2D>(eigen_tensor_a->dimension(0), eigen_tensor_a->dimension(1));
            sum_eigen<fTensor2DPtr>(eigen_tensor_a, eigen_tensor_b, eigen_tensor_c);
            nifpp::make_tensor_2d<float, fTensor2D>(env, nerltensor_bin, eigen_tensor_c);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_DOUBLE:
        {
            dTensor2DPtr eigen_tensor_a;
            dims = nifpp::get_tensor_2d<double,dTensor2DPtr,dTensor2D>(env, argv[ARG_BINARY_A], eigen_tensor_a); //TODO try use the 3d 
            dTensor2DPtr eigen_tensor_b;
            nifpp::get_tensor_2d<double,dTensor2DPtr,dTensor2D>(env, argv[ARG_BINARY_B], eigen_tensor_b);
            
            dTensor2DPtr eigen_tensor_c = make_shared<dTensor2D>(eigen_tensor_a->dimension(0), eigen_tensor_a->dimension(1));
            sum_eigen<dTensor2DPtr>(eigen_tensor_a, eigen_tensor_b, eigen_tensor_c);
            
            nifpp::make_tensor_2d<double, dTensor2D>(env, nerltensor_bin, eigen_tensor_c);
            
            return_tuple =  { nerltensor_bin , nifpp::make(env, nerltensors_type) };
            break;
        }
        case ATOM_INT32:
        {
            throw("unsuported type");
            break;
        }
        case ATOM_INT16:
        {
            throw("unsuported type");
            break;
        }

    }

    return nifpp::make(env, return_tuple);
}
