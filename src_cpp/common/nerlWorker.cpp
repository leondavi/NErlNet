#include "nerlWorker.h"

using namespace nerlnet;

NerlWorker::NerlWorker(int model_type, std::string &model_args_str , std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                    float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                    int loss_method, std::string &loss_args_str, int distributed_system_type, std::string &distributed_system_args_str)
{
    _model_type = model_type;
    _model_args_str = model_args_str;
    _learning_rate = learning_rate;
    _epochs = epochs;
    _optimizer_type = optimizer_type;
    _loss_method = loss_method;
    _loss_args_str = loss_args_str;
    _distributed_system_type = distributed_system_type;
    _distributed_system_args_str = distributed_system_args_str;
    _nerl_layers_linked_list = parse_layers_input(layer_sizes_str,layer_types_list,layers_functionality);
   // std::cout << "NerlWorker created" << std::endl;
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
        int layer_size = layer_sizes_params[i].dimx;
        int layer_functionality = std::stoi(layers_functionality_strs_vec[i]);
        std::vector<int> layer_dims = {layer_sizes_params[i].dimx,
        layer_sizes_params[i].dimy,layer_sizes_params[i].dimz};

        switch(layer_type)
        {
            case LAYER_TYPE_POOLING:
            {
                LayerSizingParams_t params = layer_sizes_params[i];
                std::vector<int>pooling_dims = params.get_ext_params(params.KERNEL_SIZE);
                std::vector<int>stride_dims  = params.get_ext_params(params.STRIDE_SIZE);
                std::vector<int>padding_dims = params.get_ext_params(params.PADDING_SIZE);
                nerl_layers_vec[i] = std::make_shared<NerlLayerPooling>(layer_type,layer_dims,layer_functionality, 
                pooling_dims, stride_dims,padding_dims);
                break;  
            }
            case LAYER_TYPE_CONV:
            {
                LayerSizingParams_t params = layer_sizes_params[i];
                std::vector<int>kernel_dims = params.get_ext_params(params.KERNEL_SIZE);
                std::vector<int>stride_dims = params.get_ext_params(params.STRIDE_SIZE);
                std::vector<int>padding_dims = params.get_ext_params(params.PADDING_SIZE);
                std::vector<int>type_conv    = params.get_ext_params(params.IS_VALID);
          //      std::cout << "type_conv 0: " << type_conv[0] << std::endl;
             //   std::cout << "type_conv 1: " << type_conv[1] << std::endl;
                nerl_layers_vec[i] = std::make_shared<NerlLayerCNN>(layer_type, layer_dims, layer_functionality, kernel_dims, stride_dims, padding_dims,type_conv);
                break; 
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


