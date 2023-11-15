#include "nerlLayer.h"

namespace nerlnet
{

// ----- NerlLayer -----
NerlLayer::NerlLayer(int layer_type, iTensor1DPtr &layer_size, int layer_functionality)
{
    _layer_type = layer_type;
    _layer_size = layer_size;
    _layer_functionality = layer_functionality;
}

NerlLayer::~NerlLayer()
{
}


// ----- CNN Layer -----
NerlLayerCNN::NerlLayerCNN(int layer_type, iTensor1DPtr &layer_size, int layer_functionality,
                           int kernel_size, int stride_size, int padding_size) : NerlLayer(layer_type, layer_size, layer_functionality)
{
    _kernel_size = kernel_size;
    _stride_size = stride_size;
    _padding_size = padding_size;
}

}