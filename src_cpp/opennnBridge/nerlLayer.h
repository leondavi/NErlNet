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

    void get_next_layer_ptr( std::shared_ptr<NerlLayer> &next_layer_ptr) {}; //TODO
    void get_prev_layer_ptr( std::shared_ptr<NerlLayer> &prev_layer_ptr) {}; //TODO

    private:
    std::shared_ptr<NerlLayer> _next_layer;
    std::shared_ptr<NerlLayer> _prev_layer;

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