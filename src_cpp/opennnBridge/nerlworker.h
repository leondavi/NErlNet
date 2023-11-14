#pragma once

#include "nerlLayer.h"

/*
LayerSizes Input: std::vector<std::string> layerSizes 16x16k5s2,8,4,3,2
*/

class NerlWorker
{

    private:

    NerlWorker(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
               float learning_rate, int epochs, int optimizer_type, int loss_mehtod ){};
    ~NerlWorker() {};

    public:

    std::shared_ptr<NerlLayer> get_layer(std::shared_ptr<NerlLayer> next_layer);

};