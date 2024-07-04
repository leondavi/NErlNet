#include <nerltensor.h>
#include <torch/torch.h>

namespace nerlnet
{

using TorchTensor = torch::Tensor;

enum {DIMS_CASE_1D,DIMS_CASE_2D,DIMS_CASE_3D};
enum {DIMS_X_IDX,DIMS_Y_IDX,DIMS_Z_IDX,DIMS_TOTAL};

} // namespace nerlnet