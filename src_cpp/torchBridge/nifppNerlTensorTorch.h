#pragma once

#include "nerltensorTorchDefs.h"
#include "nifpp.h"

namespace nifpp
{
    using namespace nerlnet;

    struct nerltensor_dims
    {
        int dimx;
        int dimy;
        int dimz;
        int total_size;
        int dims_case;
    };

    // Declarations
    template<typename BasicType> int get_nerltensor_dims(ErlNifEnv *env , ERL_NIF_TERM bin_term, nerltensor_dims &dims_info);
    template<typename BasicType> int get_nerltensor(ErlNifEnv *env , ERL_NIF_TERM bin_term, TorchTensor &tensor, torch::ScalarType torch_dtype);

    // Definitions
    template<typename BasicType> int get_nerltensor_dims(ErlNifEnv *env , ERL_NIF_TERM bin_term, nerltensor_dims &dims_info)
    {
        ErlNifBinary bin;
        int ret = enif_inspect_binary(env, bin_term, &bin);
        assert(ret != 0);

        std::vector<BasicType> dims;
        // extract dims and data size
        dims.resize(DIMS_TOTAL);
        memcpy(dims.data(), bin.data, DIMS_TOTAL * sizeof(BasicType));

        dims_info.total_size = 1;
        for (int i=0; i < DIMS_TOTAL; i++)
        {
            dims_info.total_size *= dims[i];
            if (dims[i] > 1)
            {
                dims_info.dims_case = i;
            }
        }
        assert(("Negative Or zero value of dimension", dims_info.total_size > 0)); 


        dims_info.dimx = static_cast<int>(dims[DIMS_X_IDX]);
        dims_info.dimy = static_cast<int>(dims[DIMS_Y_IDX]);
        dims_info.dimz = static_cast<int>(dims[DIMS_Z_IDX]);
    }


    template<typename BasicType> int get_nerltensor(ErlNifEnv *env , ERL_NIF_TERM bin_term, TorchTensor &tensor, torch::ScalarType torch_dtype)
    {
        ErlNifBinary bin;
        int ret = enif_inspect_binary(env, bin_term, &bin);
        assert(ret != 0);

        // extract dims and data size
        nerltensor_dims dims_info;
        get_nerltensor_dims<BasicType>(env, bin_term, dims_info);

        switch (dims_info.dims_case)
        {
            case DIMS_CASE_1D:
            {
                tensor = torch::zeros(dims_info.dimx, torch_dtype);
                break;
            }
            case DIMS_CASE_2D:
            {
                tensor = torch::zeros({dims_info.dimx, dims_info.dimy}, torch_dtype);
                break;
            }
            case DIMS_CASE_3D:
            {
                tensor = torch::zeros({dims_info.dimx, dims_info.dimy, dims_info.dimz}, torch_dtype);
                break;
            }
        }

        assert((sizeof(BasicType) == tensor.element_size(), "Size of BasicType and torch tensor element size mismatch"));

        // copy data from nerltensor to torch tensor
        int skip_dims_bytes = (DIMS_TOTAL * sizeof(BasicType));
        std::memcpy(tensor.data_ptr(),bin.data + skip_dims_bytes, sizeof(BasicType)*tensor.numel());
    }

}