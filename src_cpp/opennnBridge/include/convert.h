#include <iostream>
#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"

//#include <unsupported/Eigen/CXX11/Tensor>
namespace convert
{

// convert from Tensor to Matrix start ------------------------------------------------------------------------------------------------
template<typename T>
using  MatrixType = Eigen::Matrix<T,Eigen::Dynamic, Eigen::Dynamic>;
   
template<typename Scalar,int rank, typename sizeType>
MatrixType<Scalar> Tensor_to_Matrix(const Eigen::Tensor<Scalar,rank> &tensor,const sizeType rows,const sizeType cols)
{
    return Eigen::Map<const MatrixType<Scalar>> (tensor.data(), rows,cols);
}
// convert from Tensor to Matrix end------------------------------------------------------------------------------------------------


/*
// convert from Matrix to Tensor start ------------------------------------------------------------------------------------------------
template<typename Scalar, typename... Dims>
Eigen::Tensor<Scalar, Eigen::Index> Matrix_to_Tensor(const MatrixType<Scalar> &matrix, Dims... dims)
{
    constexpr int rank = sizeof... (Dims);
    return Eigen::TensorMap<Eigen::Tensor<const Scalar, rank>>(matrix.data(), {dims...});
}
// convert from Matrix to Tensor end ------------------------------------------------------------------------------------------------
*/

// convert from Vector to Tensor start -------------------------------------------------------------------------------------------
template<typename T>
Eigen::Tensor<Index,1> Vector_to_Tensor(vector<T> v){
    Eigen::Tensor<Index,1> tensor (v.size());
    for(int i=0; i < (int)v.size(); i++){
        tensor(i) = (Index)v[i];
    }

    return tensor;
}
// convert from Vector to Tensor end -------------------------------------------------------------------------------------------

} //End name space

