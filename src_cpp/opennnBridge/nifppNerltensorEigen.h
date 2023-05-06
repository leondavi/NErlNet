#pragma once 

#include "definitionsNN.h"
#include "nifppEigenExtensions.h"

namespace nifpp
{
    template<typename Type>
    int get_binary(ErlNifEnv *env, ERL_NIF_TERM bin_term, std::vector<Type> &vec);

    int atom_str_to_enum(str_atom in_str);
    int get_actual_dim(std::vector<int> &dims);

    template<typename BasicType> void get_tensor_data(ErlNifEnv *env , nifpp::TERM binary_term, std::vector<BasicType> &data, std::vector<int> &dims);
    template<typename BasicType, typename EigenType3D> void get_binary_as_tensor_3D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType3D> tensor_ptr);
    template<typename BasicType, typename EigenType2D> void get_binary_as_tensor_3D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType2D> tensor_ptr);
    template<typename BasicType, typename EigenType1D> void get_binary_as_tensor_3D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType1D> tensor_ptr);

    void get_binary_as_dtensor_3D(std::vector<double> &data, std::vector<int> &dims, dTensor3DPtr tensor_ptr);
    void get_binary_as_ftensor_3D(std::vector<float> &data, std::vector<int> &dims, fTensor3DPtr tensor_ptr);
    void get_binary_as_itensor_3D(std::vector<int> &data, std::vector<int> &dims, iTensor3DPtr tensor_ptr);

    void get_binary_as_dtensor_2D(std::vector<double> &data, std::vector<int> &dims, dTensor2DPtr tensor_ptr);
    void get_binary_as_ftensor_2D(std::vector<float> &data, std::vector<int> &dims, fTensor2DPtr tensor_ptr);
    void get_binary_as_itensor_2D(std::vector<int> &data, std::vector<int> &dims, iTensor2DPtr tensor_ptr);

    void get_binary_as_dtensor_1D(std::vector<double> &data, std::vector<int> &dims, dTensor1DPtr tensor_ptr);
    void get_binary_as_ftensor_1D(std::vector<float> &data, std::vector<int> &dims, fTensor1DPtr tensor_ptr);
    void get_binary_as_itensor_1D(std::vector<int> &data, std::vector<int> &dims, iTensor1DPtr tensor_ptr);
    
    template<typename Type>
    int get_binary(ErlNifEnv *env, ERL_NIF_TERM bin_term, std::vector<Type> &vec)
    {
        ErlNifBinary bin;
        int ret = enif_inspect_binary(env, bin_term, &bin);
        if(!ret)
        {
            // a binary either, so fail.
            return 0;
        }
        vec = std::vector<Type>(bin.data, bin.data + bin.size / sizeof(Type));
        return ret;
    }

   

    int atom_str_to_enum(str_atom in_str)
    {
        if (in_str == "float")
        {
            return ATOM_FLOAT;
        }
        else if (in_str == "double")
        {
            return ATOM_DOUBLE;
        }
        else if (in_str == "int32")
        {
            return ATOM_INT32;
        }
        else if (in_str == "int16")
        {
            return ATOM_INT16;
        }
        throw("Type is unsupported, update atom_str_to_enum and enum in nifppEigenExtensions.h");
    }


    template<typename BasicType> void get_tensor_data(ErlNifEnv *env , ERL_NIF_TERM bin_term, std::vector<BasicType> &data, std::vector<BasicType> &dims_orig, std::vector<int> &dims)
    {
        ErlNifBinary bin;
        int ret = enif_inspect_binary(env, bin_term, &bin);
        assert(ret != 0);

        dims_orig.resize(DIMS_TOTAL);
        dims.resize(DIMS_TOTAL);
        memcpy(dims_orig.data(), bin.data, DIMS_TOTAL * sizeof(BasicType));
        int total_data_size = 1;

        for (int i=0; i < DIMS_TOTAL; i++)
        {
            assert(("Negative Or zero value of dimension",dims_orig[i] > 0)); 
            dims[i] = static_cast<int>(dims_orig[i]);
            total_data_size *= dims_orig[i];
        }

        data.resize(total_data_size);
        memcpy(data.data() , bin.data + DIMS_TOTAL * sizeof(BasicType), total_data_size * sizeof(BasicType) );
    }

    int get_actual_dim(std::vector<int> &dims)
    {
        for (int i = dims.size() - 1; i > 0; i--)
        {
            if (dims[i] != 1)
            {
                i + 1;
            }
        }
        return 1;
    }

  

    template<typename BasicType, typename EigenType3D> inline void get_binary_as_tensor_3D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType3D> tensor_ptr)
    {
        
              

    }

    template<typename T> struct reinterprated_mat {
    T* data;
    DenseIndex rows, cols;
    Matrix<T, Dynamic, Dynamic, ColMajor>& asMatrix() {
        return reinterpret_cast<Matrix<T, Dynamic, Dynamic, ColMajor>&>(*this);
    }
    };

    template<typename BasicType, typename EigenType2D> inline void get_binary_as_tensor_2D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType2D> tensor_ptr)
    {
        // inefficient implementation still better than old fashion method
      //  tensor_ptr = make_shared<EigenType2D>(dims[DIMS_X_IDX], dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
        Eigen::Matrix<BasicType,  Eigen::Dynamic , Eigen::Dynamic> mat(dims[DIMS_X_IDX], dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
        Eigen::Matrix<BasicType,  Eigen::Dynamic , Eigen::Dynamic> mat_tr;
        memcpy(mat.data(), data.data(), data.size() * sizeof(BasicType));
        mat_tr = mat.transpose();
        //auto mapped = Eigen::TensorMap<Eigen::Tensor<BasicType, 2, Eigen::RowMajor>>(mat_tr.data(),dims[DIMS_X_IDX], dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
        memcpy(tensor_ptr->data(),mat_tr.data(), data.size() * sizeof(BasicType));// Eigen::TensorLayoutSwapOp<Eigen::Tensor<BasicType, 2, Eigen::RowMajor>>(mapped);
        //std::cout<<"tensor ptr "<<*tensor_ptr<<std::endl;
    }

    template<typename BasicType, typename EigenType1D> inline void get_binary_as_tensor_1D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType1D> tensor_ptr)
    {
        tensor_ptr = make_shared<EigenType1D>(dims[DIMS_X_IDX] * dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
        memcpy(tensor_ptr->data(), data.data(),  data.size() * sizeof(BasicType));
    }

    void get_binary_as_dtensor_3D(std::vector<double> &data, std::vector<int> &dims, dTensor3DPtr tensor_ptr)
    {
        get_binary_as_tensor_3D<double,dTensor3D>(data,dims,tensor_ptr);
    }
    void get_binary_as_ftensor_3D(std::vector<float> &data, std::vector<int> &dims, fTensor3DPtr tensor_ptr)
    {
        get_binary_as_tensor_3D<float,fTensor3D>(data,dims,tensor_ptr);
    }
    void get_binary_as_itensor_3D(std::vector<int> &data, std::vector<int> &dims, iTensor3DPtr tensor_ptr)
    {
      //  get_binary_as_tensor_3D<int,iTensor3D>(data,dims,tensor_ptr);
    }

    void get_binary_as_dtensor_2D(std::vector<double> &data, std::vector<int> &dims, dTensor2DPtr tensor_ptr)
    {
        get_binary_as_tensor_2D<double,dTensor2D>(data,dims,tensor_ptr);
    }
    void get_binary_as_ftensor_2D(std::vector<float> &data, std::vector<int> &dims, fTensor2DPtr tensor_ptr)
    {
        get_binary_as_tensor_2D<float,fTensor2D>(data,dims,tensor_ptr);
    }
    void get_binary_as_itensor_2D(std::vector<int> &data, std::vector<int> &dims, iTensor2DPtr tensor_ptr)
    {
       // get_binary_as_tensor_2D<int,iTensor2D>(data,dims,tensor_ptr);
    }

    void get_binary_as_dtensor_1D(std::vector<double> &data, std::vector<int> &dims, dTensor1DPtr tensor_ptr)
    {
        get_binary_as_tensor_1D<double,dTensor1D>(data,dims,tensor_ptr);
    }
    void get_binary_as_ftensor_1D(std::vector<float> &data, std::vector<int> &dims, fTensor1DPtr tensor_ptr)
    {
        get_binary_as_tensor_1D<float,fTensor1D>(data,dims,tensor_ptr);
    }
    void get_binary_as_itensor_1D(std::vector<int> &data, std::vector<int> &dims, iTensor1DPtr tensor_ptr)
    {
       // get_binary_as_tensor_1D<int,iTensor1D>(data,dims,tensor_ptr);
    }

}