#pragma once

#include <memory>
#include <vector>

namespace nerlnet
{

class NerlLayer
{
    public:
    NerlLayer(int layer_type, int layer_size, int layer_functionality);
    ~NerlLayer();

    void get_next_layer_ptr( std::shared_ptr<NerlLayer> &next_layer_ptr) {}; //TODO
    void get_prev_layer_ptr( std::shared_ptr<NerlLayer> &prev_layer_ptr) {}; //TODO

    void set_next_layer(std::shared_ptr<NerlLayer> &next_layer) {_next_layer = next_layer;};
    void set_prev_layer(std::shared_ptr<NerlLayer> &prev_layer) {_prev_layer = prev_layer;};

    private:
    std::shared_ptr<NerlLayer> _next_layer;
    std::shared_ptr<NerlLayer> _prev_layer;

    int _layer_type;
    int _layer_size;
    int _layer_functionality;
};


class NerlLayerCNN : public NerlLayer
{
    public:

    NerlLayerCNN(int layer_type, std::vector<int> &layers_dims, int layer_functionality,
                 int kernel_size, int stride_size, int padding_size);
    ~NerlLayerCNN();

    private:
    std::shared_ptr<int> _layers_dims;
    int _kernel_size;
    int _stride_size;
    int _padding_size;
};

} // namespace nerlnet