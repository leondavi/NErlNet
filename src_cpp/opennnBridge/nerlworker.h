#pragma once

#include "eigenTensorTypes.h"

/*
LayerSizes Input: std::vector<std::string> layerSizes 16x16k5s2,8,4,3,2


*/

typedef struct LayerSize
{
    int nothing; // TODO Nadav and Ori
}LayerSize;

class NerlWorker
{

    private:

    NerlWorker(int model_type, std::string &layer_sizes_str, iTensor1DPtr layer_types_list, iTensor1DPtr layers_functionality, float learning_rate, int epochs, int optimizer_type, int loss_mehtod );
    ~NerlWorker();

    public:

    LayerSize get_layer(void* next_layer);

};