#pragma once

#include <memory>
#include <vector>
#include "common_definitions.h"
#include "utilities.h"
#include "worker_definitions_ag.h"

namespace nerlnet
{

class NerlLayer
{
    public:
    NerlLayer(int layer_type, std::vector<int> &layers_dims, int layer_functionality);
    virtual ~NerlLayer();

    std::shared_ptr<NerlLayer> get_next_layer_ptr() {return _next_layer;};
    std::shared_ptr<NerlLayer> get_prev_layer_ptr() {return _prev_layer;};

    void set_next_layer(std::shared_ptr<NerlLayer> &next_layer) {_next_layer = next_layer;};
    void set_prev_layer(std::shared_ptr<NerlLayer> &prev_layer) {_prev_layer = prev_layer;};

    bool is_last() {return nerlnet_utilities::shared_ptr_uninitialized(_next_layer);};
    bool is_first() {return nerlnet_utilities::shared_ptr_uninitialized(_prev_layer);};

    int get_layer_type() {return _layer_type;};
    void get_layer_size(std::vector<int> &layers_dims) {layers_dims = _layers_dims;};
    int get_dim_size(int dim_idx) {return _layers_dims[dim_idx];}; // index 0 is the first dim
    int get_numof_dims() {return _layers_dims.size();};
    int get_layer_functionality() {return _layer_functionality;};

    protected:
    std::shared_ptr<NerlLayer> _next_layer;
    std::shared_ptr<NerlLayer> _prev_layer;

    std::vector<int> _layers_dims;
    int _layer_type;
    int _layer_functionality;
};


class NerlLayerPooling : public NerlLayer
{
    public:

    NerlLayerPooling(int layer_type, std::vector<int> &layers_dims, int layer_functionality, std::vector<int> &pooling_dims);
    ~NerlLayerPooling();

    void get_pooling_dims(std::vector<int> &pooling_dims) {pooling_dims = this->pooling_dims;};

    private:
    std::vector<int> pooling_dims; //TODO


};

class NerlLayerCNN : public NerlLayer
{
    public:

    NerlLayerCNN(int layer_type, std::vector<int> &layers_dims, int layer_functionality,
                 std::vector<int> kernel_size, std::vector<int> &stride_dims, std::vector<int> padding_size);
    virtual ~NerlLayerCNN();

    int get_dim_kernel_size(int dim_idx) {return _kernel_size[dim_idx];}; // index 0 is the first dim
    int get_stride(int dim_idx) {return _stride_dims[dim_idx];}; // index 0 is the first dim

    private:
    std::vector<int> _kernel_size;
    std::vector<int> _stride_dims;
    std::vector<int> _padding_size;
};

} // namespace nerlnet