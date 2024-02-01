#include "nerlWorker.h"

using namespace nerlnet;

NerlWorker::NerlWorker(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                    float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                    int loss_method, int distributed_system_type, std::string &distributed_system_args_str)
{
    _model_type = model_type;
    _learning_rate = learning_rate;
    _epochs = epochs;
    _optimizer_type = optimizer_type;
    _loss_method = loss_method;
    _distributed_system_type = distributed_system_type;
    _distributed_system_args_str = distributed_system_args_str;
    _nerl_layers_linked_list = parse_layers_input(layer_sizes_str,layer_types_list,layers_functionality);
}

NerlWorker::~NerlWorker()
{
}

/**
 * @brief parse the input of the layers and create a linked list of NerlLayer objects
 * 
 * @param layer_sizes_str comma separated list of layer sizes
 * @param layer_types_list comma separated list of layer types
 * @param layers_functionality comma separated list of layer functionality codes
 * 
 * @return pointer to the first layer of the NerlLayer linked list
*/
std::shared_ptr<NerlLayer> NerlWorker::parse_layers_input(std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality)
{
 //   std::vector<std::string> layer_sizes_strs_vec = nerlnet_utilities::split_strings_by_comma(layer_sizes_str);
    std::vector<std::string> layer_types_strs_vec = nerlnet_utilities::split_strings_by_comma(layer_types_list);
    std::vector<std::string> layers_functionality_strs_vec = nerlnet_utilities::split_strings_by_comma(layers_functionality);
    std::vector<int> layer_types_vec;
    layer_types_vec.resize(layer_types_strs_vec.size());
    for (size_t i = 0; i < layer_types_vec.size(); i++)
    {
        layer_types_vec[i] = std::stoi(layer_types_strs_vec[i]);
    }
    std::vector<LayerSizingParams_t> layer_sizes_params;

    parse_layer_sizes_str(layer_sizes_str, layer_types_vec, layer_sizes_params);

    std::vector<std::shared_ptr<NerlLayer>> nerl_layers_vec;
    nerl_layers_vec.resize(layer_sizes_params.size());
    for (int i = 0; i < layer_sizes_params.size(); i++)
    {
        int layer_type = std::stoi(layer_types_strs_vec[i]);
        // TODO Ori and Nadav add CNN extension to NerlLayer
        int layer_size = layer_sizes_params[i].dimx;
        int layer_functionality = std::stoi(layers_functionality_strs_vec[i]);

        std::vector<int> layer_dims = {layer_size}; //TODO

        switch(layer_type)
        {
            case LAYER_TYPE_POOLING:
            {
                break; //TODO Ori and Nadav add pooling layer
            }
            case LAYER_TYPE_CNN:
            {
                break; //TODO Ori and Nadav add CNN layer
            }
            default:
            {
                nerl_layers_vec[i] = std::make_shared<NerlLayer>(layer_type, layer_dims, layer_functionality);
                break;
            }
        }
    }

    for (size_t i = 1; i < nerl_layers_vec.size(); i++)
    {
       nerl_layers_vec[i-1]->set_next_layer(nerl_layers_vec[i]);
       nerl_layers_vec[i]->set_prev_layer(nerl_layers_vec[i-1]);
    }
    
   return nerl_layers_vec.front();
}


