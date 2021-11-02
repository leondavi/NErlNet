/*****************************************************
 * Author: David Leon
 * 29/10/2021
 * 
 *****************************************************/ 

#pragma once 

#include <memory>
#include "nifpp.h"
#include "Eigen/Core"
#include "unsupported/Eigen/CXX11/Tensor"

#define MAX_SUPPORTED_DIMS 3

#define EXCEPTION_STR_INVALID_ERL_TENSOR "Invalid Erlang Tensor (List)"

namespace nifppEigenExtensions
{
    /**
     * @brief 
     * This class sets the max supported dims
     * Working with dims is hardcoded for faster conversions. 
     */
    class dims
    {
        public:
        uint32_t x;
        uint32_t y;
        uint32_t z;
        uint32_t xyz;

        dims(const std::vector<int> &dimsVec)
        {
            enum {DIMS_X_IDX,DIMS_Y_IDX,DIMS_Z_IDX,DIMS_TOTAL};
            if(dimsVec.size() == DIMS_TOTAL)
            {
                x = dimsVec[DIMS_X_IDX];
                y = dimsVec[DIMS_Y_IDX];
                z = dimsVec[DIMS_Z_IDX];
                xyz = x * y * z;
            }
        }
        dims(uint32_t dimX = 1, uint32_t dimY =1, uint32_t dimZ =1): x(dimX), y(dimY), z(dimZ) { xyz = x * y * z; };
        uint32_t totalDimsSize() { return xyz; }
    };

    template<typename Type> int get(ErlNifEnv *env, ERL_NIF_TERM term, std::shared_ptr<Eigen::Tensor<Type,3>> tensorPtr);
    template<typename Type> nifpp::TERM make(ErlNifEnv *env, std::shared_ptr<Eigen::Tensor<Type,3>> tensorPtr);

    /**
     * @brief 
     * 
     * term template is an erlange list 
     * 
     * @tparam Type 
     * @tparam Dims 
     * @param env 
     * @param term - Erlang list as follows: [DimX,DimY,DimZ,Flat-Data] all floats
     * @param tensorPtr 
     * @return int 
     */
    template<typename Type> int get(ErlNifEnv *env, ERL_NIF_TERM term, std::shared_ptr<Eigen::Tensor<Type,MAX_SUPPORTED_DIMS>> tensorPtr)
    {
        unsigned len;
        int var;
        if(!enif_is_list(env, term)) return 0;
        int ret = enif_get_list_length(env, term, &len);
        if(!ret) return 0;
              
        ERL_NIF_TERM head, tail;

        // Dimensions Read 
        std::vector<int> dimsVector[MAX_SUPPORTED_DIMS];

        for(int i=0; i<MAX_SUPPORTED_DIMS; i++)
        {
            if (enif_get_list_cell(env, tail, &head, &tail))
            {
                if (!enif_get_list_cell(env, tail, &head, &tail))
                    throw  std::runtime_error(EXCEPTION_STR_INVALID_ERL_TENSOR);
                if(!nifpp::get(env, head, var)) return 0;
                dimsVector[i] = static_cast<int>(var);
            }
            else
            {
                throw std::runtime_error(EXCEPTION_STR_INVALID_ERL_TENSOR);
            }
        }
        dims tensorDims(dimsVector);
        if ((len - MAX_SUPPORTED_DIMS) != tensorDims.xyz) // length = numOfDims + x*y*z
        {
            throw std::runtime_error(EXCEPTION_STR_INVALID_ERL_TENSOR);
        }
      
        tensorPtr = std::make_shared<Eigen::Tensor<Type,MAX_SUPPORTED_DIMS>>(tensorDims.xyz,1,1); // we start with a flat tensor to copy the data easily

        for(int idx = 0; idx < tensorDims.xyz; idx++)
        {
            if (enif_get_list_cell(env, tail, &head, &tail))
            {
                if(!get(env, head, *tensorPtr(idx,1,1))) return 0;
            }
            else
            {
                throw  std::runtime_error(EXCEPTION_STR_INVALID_ERL_TENSOR);
            }
        }

        *tensorPtr = *tensorPtr.reshape(tensorDims.x,tensorDims.y,tensorDims.z);
    }

    template<typename Type> nifpp::TERM make(ErlNifEnv *env, std::shared_ptr<Eigen::Tensor<Type,MAX_SUPPORTED_DIMS>> tensorPtr)
    {
        const auto& d = tensorPtr->dimensions();
        std::vector<float> listRepresentation(&d[0], d.data() + MAX_SUPPORTED_DIMS);
        dims tensDims(listRepresentation);
        std::vector<float> tensorVec(tensorPtr->data(), tensorPtr->data()+tensDims.xyz);
        listRepresentation.insert( listRepresentation.end(), 
                                   std::make_move_iterator(tensorVec.begin()), std::make_move_iterator(tensorVec.end()));

        return nifpp::make(env,listRepresentation);
    }
}
