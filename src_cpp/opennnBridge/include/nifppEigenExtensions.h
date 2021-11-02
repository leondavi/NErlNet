/*****************************************************
 * Author: David Leon
 * 29/10/2021
 * 
 *****************************************************/ 

#pragma once 

#include "nifpp.h"
#include "Eigen/Core"
#include "unsupported/Eigen/CXX11/Tensor"

#define MAX_SUPPORTED_DIMS 3

#define EXCEPTION_STR_INVALID_ERL_TENSOR "Invalid Erlang Tensor (List)"

namespace nifpp
{
    
    /**
     * @brief 
     * This class sets the max supported dims
     * Working with dims is hardcoded for faster conversions. 
     */
    class dims
    {
        public:
        int x;
        int y;
        int z;
        int xyz;

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
        dims(int dimX = 1, int dimY =1, int dimZ =1): x(dimX), y(dimY), z(dimZ) { xyz = x * y * z; };
        int totalDimsSize() { return xyz; }
    };

    template<typename Type>
    using Tensor3D = Eigen::Tensor<Type,MAX_SUPPORTED_DIMS>;

    template<typename Type> int getTensor(ErlNifEnv *env, ERL_NIF_TERM term, Tensor3D<Type> &tensor);
    template<typename Type> TERM makeTensor(ErlNifEnv *env, const Tensor3D<Type> &tensor);


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
    template<typename Type> int getTensor(ErlNifEnv *env, ERL_NIF_TERM term, Tensor3D<Type> &tensor)
    {
        unsigned len;
        Type var;
        if(!enif_is_list(env, term)) return 0;
        int ret = enif_get_list_length(env, term, &len);
        if(!ret) return 0;
              
        ERL_NIF_TERM head, tail;
        tail = term; //very important to initalize the tail as whole list

        // Dimensions Read 
        std::vector<int> dimsVector(MAX_SUPPORTED_DIMS);
        std::cout<<"A"<<std::endl;
        for(int i=0; i<MAX_SUPPORTED_DIMS; i++)
        {
            if (enif_get_list_cell(env, tail, &head, &tail))
            {
                if(!get(env, head, var)) 
                {

                    std::cout<<"get Error"<<std::endl;
                    return 0;
                }
                std::cout<<"var dims: "<<var<<std::endl;
                dimsVector[i] = static_cast<int>(var);
            }
            else
            {
                throw std::runtime_error(EXCEPTION_STR_INVALID_ERL_TENSOR);
            }
        }
        std::cout<<"B"<<std::endl;
        dims tensorDims(dimsVector);
        if ((len - MAX_SUPPORTED_DIMS) != tensorDims.xyz) // length = numOfDims + x*y*z
        {
            throw std::runtime_error(EXCEPTION_STR_INVALID_ERL_TENSOR);
        }
        //TODO optimization 
        Tensor<Type,1> newTensor(tensorDims.xyz); // we start with a flat tensor to copy the data easily
                std::cout<<"tensorDims.xyz: "<<tensorDims.xyz<<std::endl;

        for(int idx = 0; idx < tensorDims.xyz; idx++) //copy flat tensor 
        {
            if(enif_get_list_cell(env, tail, &head, &tail))
            {

                Type var;
                if(!get(env, head, var)) return 0; // conversion failure
                
                newTensor(idx) = var;//TODO optimization 
                std::cout<<"idx "<<idx<<" var: "<<newTensor(idx) <<std::endl;

            }
            else
            {
                throw  std::runtime_error(EXCEPTION_STR_INVALID_ERL_TENSOR);
            }
        }
        Eigen::array<int, 3> dimsArray{{tensorDims.x,tensorDims.y,tensorDims.z}};
        //reshape 
        std::cout<<"before rehsape"<<std::endl;
        newTensor.reshape(dimsArray);
        std::cout<<"after rehsape"<<std::endl;

    }

    template<typename Type> nifpp::TERM makeTensor(ErlNifEnv *env, const Tensor3D<Type> &tensor)
    {
        const auto& d = tensor.dimensions();
        std::vector<float> listRepresentation(&d[0], d.data() + MAX_SUPPORTED_DIMS);
        dims tensDims(listRepresentation);
        std::vector<float> tensorVec(tensor.data(), tensor.data()+tensDims.xyz);
        listRepresentation.insert( listRepresentation.end(), 
                                   std::make_move_iterator(tensorVec.begin()), std::make_move_iterator(tensorVec.end()));

        return make(env,listRepresentation);
    }
}
