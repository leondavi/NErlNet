#include "nerlLayer.h"
#include <numeric>

namespace nerlnet
{

// ----- NerlLayer -----
NerlLayer::NerlLayer(int layer_type, std::vector<int> &layers_dims, int layer_functionality)
{
    _layer_type = layer_type;
    _layers_dims = layers_dims;
    _layer_functionality = layer_functionality;
}

NerlLayer::~NerlLayer()
{
}

// ----- CNN Layer -----
NerlLayerCNN::NerlLayerCNN(int layer_type, std::vector<int> &layers_dims, int layer_functionality,
                 std::vector<int> kernel_size, std::vector<int> &stride_dims, std::vector<int> padding_size, std::vector<int> type_conv) :
                    NerlLayer(layer_type, layers_dims, layer_functionality)
{
    _kernel_size = kernel_size;
    _stride_dims = stride_dims;
    _padding_size = padding_size;
    _type_conv = type_conv;
}

NerlLayerCNN::~NerlLayerCNN()
{
}

NerlLayerPooling::NerlLayerPooling(int layer_type, std::vector<int> &layers_dims, int layer_functionality, 
std::vector<int> &pooling_dims, std::vector<int> &stride_dims,std::vector<int> &padding_dims) :
 NerlLayer(layer_type, layers_dims, layer_functionality)
{

    _pooling_dims = pooling_dims; 
    _stride_dims = stride_dims; 
    _padding_dims = padding_dims;
};


NerlLayerPooling::~NerlLayerPooling()
{
}
}