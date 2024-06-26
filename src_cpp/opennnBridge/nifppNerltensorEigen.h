#pragma once 

#include "nerltensor.h"
#include "eigenTensorTypes.h"
#include "nifppEigenExtensions.h"

namespace nifpp
{
    template<typename BasicType> int get_actual_dim(std::vector<BasicType> &dims);
    template<typename BasicType, typename EigenTypePtr, typename EigenType> int get_tensor_1d(ErlNifEnv *env , ERL_NIF_TERM bin_term, EigenTypePtr &tensor_ptr);
    template<typename BasicType, typename EigenTypePtr, typename EigenType> int get_tensor_2d(ErlNifEnv *env , ERL_NIF_TERM bin_term, EigenTypePtr &tensor_ptr);
    template<typename BasicType, typename EigenTypePtr, typename EigenType> int get_tensor_3d(ErlNifEnv *env , ERL_NIF_TERM bin_term, EigenTypePtr &tensor_ptr);

    template<typename BasicType, typename EigenType3D> void colmajor_to_rowmajor_3d(std::shared_ptr<EigenType3D> &tensor_ptr_col_major, std::shared_ptr<Tensor<BasicType, 3, Eigen::RowMajor>> &tensor_ptr_row_major);
    template<typename BasicType, typename EigenType2D> void colmajor_to_rowmajor_2d(std::shared_ptr<EigenType2D> &tensor_ptr_col_major, std::shared_ptr<Tensor<BasicType, 2, Eigen::RowMajor>> &tensor_ptr_row_major);
    template<typename BasicType, typename EigenType3D> void rowmajor_to_colmajor_3d(std::shared_ptr<Tensor<BasicType, 3, Eigen::RowMajor>> &tensor_ptr_row_major, std::shared_ptr<EigenType3D> &tensor_ptr_col_major);
    template<typename BasicType, typename EigenType2D> void rowmajor_to_colmajor_2d(std::shared_ptr<Tensor<BasicType, 2, Eigen::RowMajor>> &tensor_ptr_row_major, std::shared_ptr<EigenType2D> &tensor_ptr_col_major);

    template<typename BasicType, typename EigenType> void make_tensor_3d(ErlNifEnv *env , nifpp::TERM &ret_bin_term, std::shared_ptr<EigenType> &tensor_ptr, bool convert_to_rowmajor = true);
    template<typename BasicType, typename EigenType> void make_tensor_2d(ErlNifEnv *env , nifpp::TERM &ret_bin_term, std::shared_ptr<EigenType> &tensor_ptr, bool convert_to_rowmajor = true);
    template<typename BasicType, typename EigenType> void make_tensor_1d(ErlNifEnv *env , nifpp::TERM &ret_bin_term, std::shared_ptr<EigenType> &tensor_ptr);

 
    template<typename BasicType, typename EigenType3D> void colmajor_to_rowmajor_3d(std::shared_ptr<EigenType3D> &tensor_ptr_col_major, std::shared_ptr<Tensor<BasicType, 3, Eigen::RowMajor>> &tensor_ptr_row_major)
    {
       int dimx = static_cast<int>(tensor_ptr_col_major->dimension(0));
       int dimy = static_cast<int>(tensor_ptr_col_major->dimension(1));
       int dimz = static_cast<int>(tensor_ptr_col_major->dimension(2));
       tensor_ptr_row_major = std::make_shared<Tensor<BasicType, 3, Eigen::RowMajor>>(dimx , dimy, dimz);
       *tensor_ptr_row_major = tensor_ptr_col_major->swap_layout().shuffle(Eigen::make_index_list(2, 1, 0));
    }

    template<typename BasicType, typename EigenType2D> void colmajor_to_rowmajor_2d(std::shared_ptr<EigenType2D> &tensor_ptr_col_major, std::shared_ptr<Tensor<BasicType, 2, Eigen::RowMajor>> &tensor_ptr_row_major)
    {
       int dimx = static_cast<int>(tensor_ptr_col_major->dimension(0));
       int dimy = static_cast<int>(tensor_ptr_col_major->dimension(1));
       tensor_ptr_row_major = std::make_shared<Tensor<BasicType, 2, Eigen::RowMajor>>(dimx , dimy);
       *tensor_ptr_row_major = tensor_ptr_col_major->swap_layout().shuffle(Eigen::make_index_list(1, 0));
    }

    template<typename BasicType, typename EigenType3D> void rowmajor_to_colmajor_3d(std::shared_ptr<Tensor<BasicType, 3, Eigen::RowMajor>> &tensor_ptr_row_major, std::shared_ptr<EigenType3D> &tensor_ptr_col_major)
    {
       int dimx = static_cast<int>(tensor_ptr_row_major->dimension(0));
       int dimy = static_cast<int>(tensor_ptr_row_major->dimension(1));
       int dimz = static_cast<int>(tensor_ptr_row_major->dimension(2));
       tensor_ptr_col_major = std::make_shared<EigenType3D>(dimx , dimy, dimz);
       *tensor_ptr_col_major = tensor_ptr_row_major->swap_layout().shuffle(Eigen::make_index_list(2, 1, 0));
    }

    template<typename BasicType, typename EigenType2D> void rowmajor_to_colmajor_2d(std::shared_ptr<Tensor<BasicType, 2, Eigen::RowMajor>> &tensor_ptr_row_major, std::shared_ptr<EigenType2D> &tensor_ptr_col_major)
    {
       int dimx = static_cast<int>(tensor_ptr_row_major->dimension(0));
       int dimy = static_cast<int>(tensor_ptr_row_major->dimension(1));
       tensor_ptr_col_major = std::make_shared<EigenType2D>(dimx , dimy);
       *tensor_ptr_col_major = tensor_ptr_row_major->swap_layout().shuffle(Eigen::make_index_list(1, 0));
    }

    template<typename BasicType> int get_actual_dim(std::vector<BasicType> &dims)
    {
        for (int i = dims.size() - 1; i > 0; i--)
        {
            if (dims[i] != static_cast<BasicType>(1))
            {
                i + 1;
            }
        }
        return 1;
    }
    
    template<typename BasicType, typename TensorPtr> void dims_vec_from_tensor(TensorPtr &tensor_ptr, std::vector<BasicType> &dims_vec, int dims, int &tensor_total_size)
    {
        dims_vec.resize(NERL_TENSOR_DIMS);
        tensor_total_size = 1;

        for (int i=0; i < dims_vec.size(); i++)
        {
            if (i < dims)
            {
                dims_vec[i] = static_cast<BasicType>(tensor_ptr->dimension(i));
            }
            else 
            {
                dims_vec[i] = static_cast<BasicType>(1);
            }
            tensor_total_size *= dims_vec[i];
        }
    }

    template<typename BasicType, typename EigenType> void make_tensor_3d(ErlNifEnv *env , nifpp::TERM &ret_bin_term, std::shared_ptr<EigenType> &tensor_ptr, bool convert_to_rowmajor)
    {
        std::vector<BasicType> dims_vec;
        int tensor_total_size;
        dims_vec_from_tensor<BasicType,std::shared_ptr<EigenType>>(tensor_ptr, dims_vec, CASE_3D, tensor_total_size);
        
        size_t dim_size = NERL_TENSOR_DIMS * sizeof(BasicType);
        size_t data_size = tensor_total_size * sizeof(BasicType);
        nifpp::binary nifpp_bin(dim_size + data_size);
        void* data_bytes_ptr = tensor_ptr->data();

        if (convert_to_rowmajor)
        {
            std::shared_ptr<Eigen::Tensor<BasicType,CASE_3D,Eigen::RowMajor>>  tensor_ptr_rowmaj;
            colmajor_to_rowmajor_3d<BasicType, Eigen::Tensor<BasicType,CASE_3D>>(tensor_ptr, tensor_ptr_rowmaj);
            dims_vec_from_tensor<BasicType,std::shared_ptr<Eigen::Tensor<BasicType,CASE_3D,Eigen::RowMajor>>>(tensor_ptr_rowmaj, dims_vec, CASE_3D, tensor_total_size);
            data_bytes_ptr = tensor_ptr_rowmaj->data();
            // copy data
            std::memcpy(nifpp_bin.data, dims_vec.data(), dim_size);
            std::memcpy(nifpp_bin.data + dim_size, data_bytes_ptr, data_size);
        }

        ret_bin_term = nifpp::make(env, nifpp_bin);
    }


    template<typename BasicType, typename EigenType> void make_tensor_2d(ErlNifEnv *env , nifpp::TERM &ret_bin_term, std::shared_ptr<EigenType> &tensor_ptr, bool convert_to_rowmajor)
    {
        std::vector<BasicType> dims_vec;
        int tensor_total_size;
        dims_vec_from_tensor<BasicType,std::shared_ptr<EigenType>>(tensor_ptr, dims_vec, CASE_2D, tensor_total_size);
        
        size_t dim_size = NERL_TENSOR_DIMS * sizeof(BasicType);
        size_t data_size = tensor_total_size * sizeof(BasicType);
        nifpp::binary nifpp_bin(dim_size + data_size);
        void* data_bytes_ptr = tensor_ptr->data();
        if (convert_to_rowmajor)
        {
            std::shared_ptr<Eigen::Tensor<BasicType,CASE_2D,Eigen::RowMajor>>  tensor_ptr_rowmaj;
            colmajor_to_rowmajor_2d<BasicType, Eigen::Tensor<BasicType,CASE_2D>>(tensor_ptr, tensor_ptr_rowmaj);
            dims_vec_from_tensor<BasicType,std::shared_ptr<Eigen::Tensor<BasicType,CASE_2D,Eigen::RowMajor>>>(tensor_ptr_rowmaj, dims_vec, CASE_2D, tensor_total_size);
            data_bytes_ptr = tensor_ptr_rowmaj->data();
            
            // copy data
            std::memcpy(nifpp_bin.data, dims_vec.data(), dim_size);
            std::memcpy(nifpp_bin.data + dim_size, data_bytes_ptr, data_size);
        }

        ret_bin_term = nifpp::make(env, nifpp_bin);
    }

    template<typename BasicType, typename EigenType> void make_tensor_1d(ErlNifEnv *env , nifpp::TERM &ret_bin_term, std::shared_ptr<EigenType> &tensor_ptr)
    {
        std::vector<BasicType> dims_vec;
        int tensor_total_size;
        dims_vec_from_tensor<BasicType,std::shared_ptr<EigenType>>(tensor_ptr, dims_vec, CASE_1D, tensor_total_size);
        
        size_t dim_size = NERL_TENSOR_DIMS * sizeof(BasicType);
        size_t data_size = tensor_total_size * sizeof(BasicType);
        nifpp::binary nifpp_bin(dim_size + data_size);
        void* data_bytes_ptr = tensor_ptr->data();

        // copy data
        std::memcpy(nifpp_bin.data, dims_vec.data(), dim_size);
        std::memcpy(nifpp_bin.data + dim_size, data_bytes_ptr, data_size);
        ret_bin_term = nifpp::make(env, nifpp_bin);
    }

    

    /**
     * Returns a colMajor tensor from binary term which is the default of Eigen
    */
    template<typename BasicType, typename EigenTypePtr, typename EigenType> int get_tensor_1d(ErlNifEnv *env , ERL_NIF_TERM bin_term, EigenTypePtr &tensor_ptr)
    {
        ErlNifBinary bin;
        int ret = enif_inspect_binary(env, bin_term, &bin);
        assert((ret != 0, "not a binary"));

        std::vector<BasicType> dims;
        // extract dims and data size
        dims.resize(DIMS_TOTAL);
        memcpy(dims.data(), bin.data, DIMS_TOTAL * sizeof(BasicType));
        
        int total_data_size = 1;
        for (int i=0; i < DIMS_TOTAL; i++)
        {
            total_data_size *= dims[i];
        }
        assert(("Negative Or zero value of dimension",total_data_size > 0)); 

        int dim_case = get_actual_dim<BasicType>(dims);

        int dimx = static_cast<int>(dims[DIMS_X_IDX] * dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
        tensor_ptr = std::make_shared<Eigen::Tensor<BasicType, CASE_1D>>(dimx);  
        Eigen::Tensor<BasicType, CASE_1D, Eigen::RowMajor> row_maj_tensor(dimx); 
        memcpy(row_maj_tensor.data(), bin.data +  (DIMS_TOTAL * sizeof(BasicType)) , total_data_size * sizeof(BasicType));
        *tensor_ptr = row_maj_tensor.swap_layout();
        return CASE_1D;
    } 

    template<typename BasicType, typename EigenTypePtr, typename EigenType> int get_tensor_2d(ErlNifEnv *env , ERL_NIF_TERM bin_term, EigenTypePtr &tensor_ptr)
    {
        ErlNifBinary bin;
        int ret = enif_inspect_binary(env, bin_term, &bin);
        assert(ret != 0);

        std::vector<BasicType> dims;
        // extract dims and data size
        dims.resize(DIMS_TOTAL);
        memcpy(dims.data(), bin.data, DIMS_TOTAL * sizeof(BasicType));
        
        int total_data_size = 1;
        for (int i=0; i < DIMS_TOTAL; i++)
        {
            total_data_size *= dims[i];
        }
        assert(("Negative Or zero value of dimension",total_data_size > 0)); 

        int dim_case = get_actual_dim<BasicType>(dims);

        int dimx = static_cast<int>(dims[DIMS_X_IDX]);
        int dimy = static_cast<int>(dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
        tensor_ptr = std::make_shared<Eigen::Tensor<BasicType, CASE_2D>>(dimx,dimy);  
        std::shared_ptr<Eigen::Tensor<BasicType, CASE_2D, Eigen::RowMajor>> row_maj_tensor = std::make_shared<Eigen::Tensor<BasicType, CASE_2D, Eigen::RowMajor>>(dimx, dimy); 
        memcpy(row_maj_tensor->data(), bin.data +  (DIMS_TOTAL * sizeof(BasicType)) , total_data_size * sizeof(BasicType));
        rowmajor_to_colmajor_2d<BasicType,Eigen::Tensor<BasicType, CASE_2D>>(row_maj_tensor, tensor_ptr);
        return CASE_2D;
    } 

   template<typename BasicType, typename EigenTypePtr, typename EigenType> int get_tensor_3d(ErlNifEnv *env , ERL_NIF_TERM bin_term, EigenTypePtr &tensor_ptr)
    {
        ErlNifBinary bin;
        int ret = enif_inspect_binary(env, bin_term, &bin);
        assert(ret != 0);

        std::vector<BasicType> dims;
        // extract dims and data size
        dims.resize(DIMS_TOTAL);
        memcpy(dims.data(), bin.data, DIMS_TOTAL * sizeof(BasicType));
        
        int total_data_size = 1;
        for (int i=0; i < DIMS_TOTAL; i++)
        {
            total_data_size *= dims[i];
        }
        assert(("Negative Or zero value of dimension",total_data_size > 0)); 

        int dim_case = get_actual_dim<BasicType>(dims);

        int dimx = static_cast<int>(dims[DIMS_X_IDX]);
        int dimy = static_cast<int>(dims[DIMS_Y_IDX]);
        int dimz = static_cast<int>(dims[DIMS_Z_IDX]);
        tensor_ptr = std::make_shared<Eigen::Tensor<BasicType, CASE_3D>>(dimx,dimy,dimz);

        std::shared_ptr<Eigen::Tensor<BasicType, CASE_3D, Eigen::RowMajor>> row_maj_tensor = std::make_shared<Eigen::Tensor<BasicType, CASE_3D, Eigen::RowMajor>>(dimx, dimy); 
        memcpy(row_maj_tensor->data(), bin.data +  (DIMS_TOTAL * sizeof(BasicType)) , total_data_size * sizeof(BasicType));
        rowmajor_to_colmajor_3d<BasicType,Eigen::Tensor<BasicType, CASE_2D>>(row_maj_tensor, tensor_ptr);
        return CASE_3D;
    } 

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
}