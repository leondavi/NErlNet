#pragma once

#include <nerltensor.h>
#include <torch/torch.h>
#include <unordered_map>
#include <string>

namespace nerlnet
{

using TorchTensor = torch::Tensor;

enum {DIMS_CASE_1D,DIMS_CASE_2D,DIMS_CASE_3D};
enum {DIMS_X_IDX,DIMS_Y_IDX,DIMS_Z_IDX,DIMS_TOTAL};

const std::map<std::string, c10::ScalarType> torch_dtype_map = {
    {"float", c10::ScalarType::Float},
    {"double", c10::ScalarType::Double},
    {"int", c10::ScalarType::Int},
    {"long", c10::ScalarType::Long},
    {"bool", c10::ScalarType::Bool},
    {"uint8", c10::ScalarType::Byte},
    {"int8", c10::ScalarType::Char},
    {"int16", c10::ScalarType::Short},
    {"int32", c10::ScalarType::Int},
    {"int64", c10::ScalarType::Long},
    {"float16", c10::ScalarType::Half},
    {"float32", c10::ScalarType::Float},
    {"float64", c10::ScalarType::Double},
    {"qint8", c10::ScalarType::QInt8},
    {"quint8", c10::ScalarType::QUInt8},
    {"qint32", c10::ScalarType::QInt32},
    {"bfloat16", c10::ScalarType::BFloat16},
    {"bool", c10::ScalarType::Bool},
    {"uint8", c10::ScalarType::Byte},
    {"int8", c10::ScalarType::Char},
    {"int16", c10::ScalarType::Short},
    {"int32", c10::ScalarType::Int},
    {"int64", c10::ScalarType::Long},
    {"float16", c10::ScalarType::Half},
    {"float32", c10::ScalarType::Float},
    {"float64", c10::ScalarType::Double}};

inline c10::ScalarType get_torch_dtype(const std::string &dtype_str)
{
    assert (torch_dtype_map.find(dtype_str) != torch_dtype_map.end());
    return torch_dtype_map.at(dtype_str);
}

} // namespace nerlnet