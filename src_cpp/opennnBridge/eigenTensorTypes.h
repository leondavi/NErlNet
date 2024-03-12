#pragma once

#include "../opennn/opennn/opennn.h"

namespace nerlnet
{

using intTensor1D = Eigen::Tensor<int,1>;
using intTensor2D = Eigen::Tensor<int,2>;
using intTensor3D = Eigen::Tensor<int,3>;

using intTensor1DPtr = std::shared_ptr<intTensor1D>;
using intTensor2DPtr = std::shared_ptr<intTensor2D>;
using intTensor3DPtr = std::shared_ptr<intTensor3D>;

using iTensor1D = Eigen::Tensor<Eigen::Index,1>;
using iTensor2D = Eigen::Tensor<Eigen::Index,2>;
using iTensor3D = Eigen::Tensor<Eigen::Index,3>;

using iTensor1DPtr = std::shared_ptr<iTensor1D>;
using iTensor2DPtr = std::shared_ptr<iTensor2D>;
using iTensor3DPtr = std::shared_ptr<iTensor3D>;

using fTensor1D = Eigen::Tensor<float,1>;
using fTensor2D = Eigen::Tensor<float,2>;
using fTensor3D = Eigen::Tensor<float,3>;

using fTensor1DPtr = std::shared_ptr<fTensor1D>;
using fTensor2DPtr = std::shared_ptr<fTensor2D>;
using fTensor3DPtr = std::shared_ptr<fTensor3D>;

using dTensor1D = Eigen::Tensor<double,1>;
using dTensor2D = Eigen::Tensor<double,2>;
using dTensor3D = Eigen::Tensor<double,3>;

using dTensor1DPtr = std::shared_ptr<dTensor1D>;
using dTensor2DPtr = std::shared_ptr<dTensor2D>;
using dTensor3DPtr = std::shared_ptr<dTensor3D>;

/**
 * Inefficient!
 * 
 * Use this only for small tensors (e.g. layer sizes)
 * 
*/
template <int Dim>
void convert_tensor_int_to_tensor_index(std::shared_ptr<Eigen::Tensor<int, Dim>> &tensor_int, std::shared_ptr<Eigen::Tensor<Eigen::Index,Dim>> &tensor_index)
{
    tensor_index = std::make_shared<Eigen::Tensor<Eigen::Index,Dim>>(tensor_int->size());
    for (int i = 0; i < tensor_int->size(); i++)
    {
        (*tensor_index)(i) = (*tensor_int)(i);
    }
}

template <typename SimpleType>
void vector_to_tensor_1d(std::vector<SimpleType> &vec, std::shared_ptr<Eigen::Tensor<SimpleType,1>> &tensor)
{
   tensor = std::make_shared<Eigen::Tensor<SimpleType,1>>(vec.size());
   std::memcpy(tensor->data(), vec.data(), vec.size() * sizeof(SimpleType));
}

template <typename SimpleType>
void vector_to_tensor_2d(std::vector<SimpleType> &vec, std::shared_ptr<Eigen::Tensor<SimpleType,2>> &tensor)
{
   tensor = std::make_shared<Eigen::Tensor<SimpleType,1>>(vec.size());
   std::memcpy(tensor->data(), vec.data(), vec.size() * sizeof(SimpleType));
}

template <typename SimpleType>
void vector_to_tensor_3d(std::vector<SimpleType> &vec, std::shared_ptr<Eigen::Tensor<SimpleType,3>> &tensor)
{
   tensor = std::make_shared<Eigen::Tensor<SimpleType,1>>(vec.size());
   std::memcpy(tensor->data(), vec.data(), vec.size() * sizeof(SimpleType));
}

} // namespace nerlnet