#pragma once

#include <string>
#include <unordered_map>
// enum defnitions
enum {NERL_TENSOR_DIMS = 3, CASE_1D = 1, CASE_2D =2, CASE_3D = 3};
enum NERLTENSOR_DATA_TYPES_NUM {ATOM_FLOAT, ATOM_DOUBLE, ATOM_INT32, ATOM_INT16, ATOM_UINT8};


static std::unordered_map<std::string, int> atom_str_to_enum_map = {
    {"float", ATOM_FLOAT},
    {"double", ATOM_DOUBLE},
    {"int32", ATOM_INT32},
    {"int16", ATOM_INT16},
    {"uint8", ATOM_UINT8}
};

// functions
inline int atom_str_to_enum(std::string &in_str)
{
    return atom_str_to_enum_map[in_str];
}