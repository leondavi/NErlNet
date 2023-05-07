#pragma once 

#include "definitionsNN.h"
#include "nifppEigenExtensions.h"

namespace nifpp
{
    template<typename Type>
    int get_binary(ErlNifEnv *env, ERL_NIF_TERM bin_term, std::vector<Type> &vec);

    int atom_str_to_enum(str_atom in_str);
 
    template<typename BasicType, typename EigenType3D> inline void colmajor_to_rowjmajor_3d(std::shared_ptr<EigenType3D> &tensor_ptr_col_major, std::shared_ptr<Tensor<BasicType, 3, RowMajor>> &tensor_ptr_row_major);
    {
       int dimx = reinterpret_cast<BasicType>(tensor_ptr_col_major->dimension(0));
       int dimy = reinterpret_cast<BasicType>(tensor_ptr_col_major->dimension(1));
       int dimz = reinterpret_cast<BasicType>(tensor_ptr_col_major->dimension(2));
       Eigen::TensorMap<Tensor<BasicType, 3, RowMajor> > row_major(tensor_ptr_col_major->data(),dimx, dimy, dimz );        
       tensor_ptr_row_major = std::make_shared(dimx , dimy , dimz);
       *tensor_ptr_row_major = row_major;
    }

    template<typename BasicType, typename EigenType2D> inline void colmajor_to_rowjmajor_2d(std::shared_ptr<EigenType2D> &tensor_ptr_col_major, std::shared_ptr<Tensor<BasicType, 3, RowMajor>> &tensor_ptr_row_major)
    {
       int dimx = reinterpret_cast<BasicType>(tensor_ptr_col_major->dimension(0));
       int dimy = reinterpret_cast<BasicType>(tensor_ptr_col_major->dimension(1));
       Eigen::TensorMap<Tensor<BasicType, 2, RowMajor> > row_major(tensor_ptr_col_major->data(),dimx, dimy);        
       tensor_ptr_row_major = std::make_shared(dimx , dimy);
       *tensor_ptr_row_major = row_major;
    }

        template<typename BasicType> inline int get_actual_dim(std::vector<BasicType> &dims)
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

    template<typename BasicType, typename EigenType> int make_tensor(ErlNifEnv *env , nifpp::TERM &ret_bin_term, int dims, std::shared_ptr<EigenType> &tensor_ptr, bool convert_to_rowmajor = true)
    {
        enum {WRONG_DIM_ERR = -1, CASE_1D = 1, CASE_2D = 2, CASE_3D = 3};
        enum {NERL_TENSOR_DIMS = 3};

        std::vector<BasicType> dims_vec;
        dims_vec.resize(NERL_TENSOR_DIMS);

        int tensor_total_size = 1;

        for (int i=0; i < dims_vec.size(); i++)
        {
            if (i < dims)
            {
                dims_vec[i] = reinterpret_cast<BasicType>(tensor_ptr->dimension(i));
            }
            else 
            {
                dims_vec[i] = reinterpret_cast<BasicType>(1);
            }
            tensor_total_size *= dims_vec[i];
        }
        
        size_t dim_size = NERL_TENSOR_DIMS *  sizeof(BasicType);
        size_t data_size = tensor_total_size * sizeof(BasicType);
        nifpp::binary nifpp_bin(dim_size + data_size);
        void* data_bytes_ptr = tensor_ptr->data();

        if (convert_to_rowmajor)
        {
            switch (dims)
            {
                case CASE_1D:
                {
                    break; // same order of values for both cases
                }
                case CASE_2D:
                {
                    std::shared_ptr<Eigen::Tensor<BasicType,CASE_2D,Eigen::RowMajor>>  tensor_ptr_rowmaj;
                    colmajor_to_rowjmajor_2d<BasicType, EigenType>(tensor_ptr, tensor_ptr_rowmaj);
                    data_bytes_ptr = tensor_ptr_rowmaj->data();
                    break;
                }
                case CASE_3D:
                {
                    std::shared_ptr<Eigen::Tensor<BasicType,CASE_3D,Eigen::RowMajor>>  tensor_ptr_rowmaj;
                    colmajor_to_rowjmajor_3d<BasicType, EigenType>(tensor_ptr, tensor_ptr_rowmaj);
                    data_bytes_ptr = tensor_ptr_rowmaj->data();
                    break;
                }
                case WRONG_DIM_ERR:
                {
                    throw("Wrong dimension!");
                    break;
                }
            }
        }

        // copy data
        std::memcpy(nifpp_bin.data, dims_vec.data(), dim_size);
        std::memcpy(nifpp_bin.data + dim_size, data_bytes_ptr, data_size);
        ret_bin_term = nifpp::make(env, nifpp_bin);
    }

    /**
     * Returns a colMajor tensor from binary term which is the default of Eigen
    */
    template<typename BasicType, typename EigenType> int get_tensor(ErlNifEnv *env , nifpp::TERM bin_term, std::shared_ptr<EigenType> &tensor_ptr)
    {
        enum {WRONG_DIM_ERR = -1, CASE_1D = 1, CASE_2D = 2, CASE_3D = 3};
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

        switch (dim_case)
        {
            case CASE_1D:
            {
                int dimx = static_cast<int>(dims[DIMS_X_IDX] * dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
                tensor_ptr = std::make_shared<EigenType>(dimx);  
                Eigen::Tensor<BasicType, CASE_1D, Eigen::RowMajor> row_maj_tensor(dimx); 
                memcpy(row_maj_tensor.data(), bin.data +  (DIMS_TOTAL * sizeof(BasicType)) , total_data_size * sizeof(BasicType));
                *tensor_ptr = row_maj_tensor.swap_layout();
                return CASE_1D;
                break;
            }
            case CASE_2D:
            {
                int dimx = static_cast<int>(dims[DIMS_X_IDX]);
                int dimy = static_cast<int>(dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
                tensor_ptr = std::make_shared<EigenType>(dimx,dimy);  
                Eigen::Tensor<BasicType, CASE_2D, Eigen::RowMajor> row_maj_tensor(dimx, dimy); 
                memcpy(row_maj_tensor.data(), bin.data +  (DIMS_TOTAL * sizeof(BasicType)) , total_data_size * sizeof(BasicType));
                *tensor_ptr = row_maj_tensor.swap_layout();
                return CASE_2D;
            }
            case CASE_3D:
            {
                int dimx = static_cast<int>(dims[DIMS_X_IDX]);
                int dimy = static_cast<int>(dims[DIMS_Y_IDX]);
                int dimz = static_cast<int>(dims[DIMS_Z_IDX]);
                tensor_ptr = std::make_shared<EigenType>(dimx,dimy,dimz);   
                Eigen::Tensor<BasicType, CASE_3D, Eigen::RowMajor> row_maj_tensor(dimx); 
                memcpy(row_maj_tensor.data(), bin.data +  (DIMS_TOTAL * sizeof(BasicType)) , total_data_size * sizeof(BasicType));
                *tensor_ptr = row_maj_tensor.swap_layout();
                return CASE_3D;
            }
        }
        return WRONG_DIM_ERR;
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



  

    template<typename BasicType, typename EigenType3D> inline void get_binary_as_tensor_3D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType3D> tensor_ptr)
    {
          
              

    }



    template<typename BasicType, typename EigenType2D> void inline get_binary_as_tensor_2D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType2D> &tensor_ptr)
    {
        Eigen::Tensor<float, 3> a; 
        
        tensor_ptr = std::make_shared<EigenType2D>(dims[DIMS_X_IDX], dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);       
        Eigen::Tensor<BasicType, 2, Eigen::RowMajor> row_maj_tensor(dims[DIMS_X_IDX], dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]); 
        memcpy(row_maj_tensor.data(),data.data(), data.size() * sizeof(BasicType));
        *tensor_ptr = row_maj_tensor.swap_layout();
        //std::cout<<"RowMajor: "<<row_maj_tensor<<std::endl<<std::endl;
        //std::cout<<"TensorPTR: "<<*tensor_ptr<<std::endl<<std::endl;
    }

    template<typename BasicType, typename EigenType1D> inline void get_binary_as_tensor_1D(std::vector<BasicType> &data, std::vector<int> &dims, std::shared_ptr<EigenType1D> tensor_ptr)
    {
        tensor_ptr = make_shared<EigenType1D>(dims[DIMS_X_IDX] * dims[DIMS_Y_IDX] * dims[DIMS_Z_IDX]);
        memcpy(tensor_ptr->data(), data.data(),  data.size() * sizeof(BasicType));
    }
}