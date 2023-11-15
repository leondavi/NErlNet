#pragma once

#include "eigenTensorTypes.h"
#include "worker_definitions_ag.h"

namespace nerlnet
{

class NerlLayer
{
    public:
    NerlLayer(int layer_type, iTensor1DPtr &layer_size, int layer_functionality);
    ~NerlLayer();

    private:
    int _layer_type;
    iTensor1DPtr _layer_size;
    int _layer_functionality;
};

class NerlLayerCNN : public NerlLayer
{
    public:

    NerlLayerCNN(int layer_type, iTensor1DPtr &layer_size, int layer_functionality,
                 int kernel_size, int stride_size, int padding_size);
    ~NerlLayerCNN();

    private:
    int _kernel_size;
    int _stride_size;
    int _padding_size;
};

} // namespace nerlnet