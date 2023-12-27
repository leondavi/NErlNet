#pragma once

#include <memory>

namespace nerlnet
{
template <class NerlWorkerType>
std::shared_ptr<NerlWorkerType> parse_model_params(std::string &model_type_str,std::string &learning_rate_str,std::string &epochs_str,
                                    std::string &optimizer_type_str,std::string &loss_method_str,std::string &distributed_system_type_str,
                                    std::string &layer_sizes_str,std::string &layer_types_str,std::string &layers_functionality_str,
                                    std::string &optimizer_args_str,std::string &distributed_system_args_str)
{
    int model_type = std::stoi(model_type_str);
    float learning_rate = std::stof(learning_rate_str);
    int epochs = std::stoi(epochs_str);
    int optimizer_type = std::stoi(optimizer_type_str);
    int loss_method = std::stoi(loss_method_str);
    int distributed_system_type = std::stoi(distributed_system_type_str);
    return std::make_shared<NerlWorkerType>(model_type, layer_sizes_str, layer_types_str, layers_functionality_str,learning_rate, epochs, optimizer_type, optimizer_args_str, loss_method, distributed_system_type, distributed_system_args_str);
}

static void parse_layer_sizes_str(std::string &layer_sizes_str, std::vector<int> &layer_sizes_parsed)
{
    enum {KERNEL_SIZE_IDX = -1, PADDING_SIZE_IDX = -2, STRIDE_SIZE_IDX = -3, LAYER_STARTING_INDEX = -4};

    // "5x5k2x2p1s1", 5,5,KERNEL_SIZE_IDX,2,2,PADDING_SIZE_IDX,1  | 
    // "5k2p1", 5,KERNEL_SIZE_IDX,2,PADDING_SIZE_IDX,1  |
    // "8", 8

    // layer_starting_indexes


    // Options: 
    // 1. Create struct of vectors of int: 
    // 2. Represent in a 1D vector and using a second vector for layer start 
    // 3. Create class

}

}