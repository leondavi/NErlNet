#pragma once

#include "../opennn/opennn/opennn.h"

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
