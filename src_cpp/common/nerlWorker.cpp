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
    std::vector<std::string> layer_sizes_strs_vec = nerlnet_utilities::split_strings_by_comma(layer_sizes_str);
    std::vector<std::string> layer_types_strs_vec = nerlnet_utilities::split_strings_by_comma(layer_types_list);
    std::vector<std::string> layers_functionality_strs_vec = nerlnet_utilities::split_strings_by_comma(layers_functionality);

    std::shared_ptr<NerlLayer> first_layer;
    std::shared_ptr<NerlLayer> prev_layer;

    assert(layer_sizes_strs_vec.size() == layer_types_strs_vec.size());
    assert(layer_sizes_strs_vec.size() == layers_functionality_strs_vec.size());

    for (int i = 0; i < layer_sizes_strs_vec.size(); i++)
    {
        std::shared_ptr<NerlLayer> next_layer;
        int layer_type = std::stoi(layer_types_strs_vec[i]);
        // TODO Ori and Nadav add CNN extension
        int layer_size = std::stoi(layer_sizes_strs_vec[i]);
        int layer_functionality = std::stoi(layers_functionality_strs_vec[i]);

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
                next_layer = std::make_shared<NerlLayer>(layer_type, layer_dims, layer_functionality);
                break;
            }
        }

        std::cout<<"layer_type: "<<layer_type<<std::endl;
        std::cout<<"layer_size: "<<layer_size<<std::endl;
        std::cout<<"layer_functionality: "<<layer_functionality<<std::endl;


        if (i == 0)
        {
            first_layer = next_layer;
        }
        else if (i > 0)
        {
            next_layer->set_prev_layer(prev_layer);
        }
        prev_layer = next_layer;
    }

    return first_layer;
}
