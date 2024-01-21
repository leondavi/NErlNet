#pragma once

#include <memory>

#include <Logger.h>
#include "utilities.h"
#include "worker_definitions_ag.h"


#define SIMPLE_PARSING -1
#define COMPLEX_PARSING -2

namespace nerlnet
{

typedef struct LayerSizingParams
{
 enum {KERNEL_SIZE = -1, PADDING_SIZE = -2, STRIDE_SIZE = -3, POOLING_SIZE= -4};
 int dimx = 1;
 int dimy = 1;
 int dimz = 1; //at the moment we support up to 3 dimensions
 std::vector<int> _ext_params; 

 std::vector<int> get_ext_params(int param_type) {
  std::vector<int> res;
  int i = 0;
  int param_extracted = false;
  int param_start = false;
  while (!param_extracted){
    if(param_start){
        param_extracted = _ext_params[i]<0;
        if(!param_extracted){
            res.push_back(_ext_params[i]);
        }
    }
    if(_ext_params[i] == param_type){
            param_start = true;
    }
    i++;
 } 
 return res;
 }
} LayerSizingParams_t;

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

static void parse_layer_sizes_str(std::string &layer_sizes_str, std::vector<int> &layers_types_vec, std::vector<LayerSizingParams_t> &out_layer_sizes_params)
{

    std::vector<std::string> layer_sizes_strs_vec = nerlnet_utilities::split_strings_by_comma(layer_sizes_str);
    out_layer_sizes_params.resize(layer_sizes_strs_vec.size()); 
    assert(layer_sizes_strs_vec.size() == out_layer_sizes_params.size());
    for (size_t i = 0; i < layer_sizes_strs_vec.size(); i++) //TODO
    {
        int layer_str_type = nerlnet_utilities::is_integer_number(layer_sizes_strs_vec[i]) ? SIMPLE_PARSING : COMPLEX_PARSING; 

        switch (layer_str_type)
        {
            case SIMPLE_PARSING:{
                out_layer_sizes_params[i].dimx = std::stoi(layer_sizes_strs_vec[i]); 
                break;
            }
           case COMPLEX_PARSING:{
                LogInfo("Complex parsing"); 
                //TODO: make sure to complete fill up the dimensions of Kernel and Padding even if they are not fully specified 
                //TODO: make sure to validate that it makes sense with the definition of convolution
                 std::regex rgx("(\\d+)x(\\d+)(x(\\d+))?k(\\d+)x(\\d+)p(\\d+)x(\\d+)s(\\d+)");
                std::smatch matches;

                if (std::regex_search(layer_sizes_strs_vec[i], matches, rgx)) {
                    enum {KERNEL_SIZE = -1, PADDING_SIZE = -2, STRIDE_SIZE = -3, POOLING_SIZE= -4}; //TODO: enums declaration shouldn't be here
                    out_layer_sizes_params[i].dimx = std::stoi(matches[1]); // dimx
                    out_layer_sizes_params[i]._ext_params.push_back(out_layer_sizes_params[i].dimx);

                    out_layer_sizes_params[i].dimy = std::stoi(matches[2]); // dimy
                    out_layer_sizes_params[i]._ext_params.push_back(out_layer_sizes_params[i].dimy);

                    if(matches[4].matched) { // if there is a third dimension
                        out_layer_sizes_params[i].dimz = std::stoi(matches[4]); // dimz
                        out_layer_sizes_params[i]._ext_params.push_back(out_layer_sizes_params[i].dimz);
                    }

                    out_layer_sizes_params[i]._ext_params.push_back(KERNEL_SIZE);
                    out_layer_sizes_params[i]._ext_params.push_back(std::stoi(matches[5]));
                    out_layer_sizes_params[i]._ext_params.push_back(std::stoi(matches[6]));
                    out_layer_sizes_params[i]._ext_params.push_back(PADDING_SIZE);
                    out_layer_sizes_params[i]._ext_params.push_back(std::stoi(matches[7]));
                    out_layer_sizes_params[i]._ext_params.push_back(std::stoi(matches[8]));
                    out_layer_sizes_params[i]._ext_params.push_back(STRIDE_SIZE);
                    out_layer_sizes_params[i]._ext_params.push_back(std::stoi(matches[9]));
                }
                break;
                }
            default:
                break;   
    
        }
    }   



                
  
            
  
    

    // "5x5k2x2p1x1s1", 5,5,KERNEL_SIZE_IDX,2,2,PADDING_SIZE_IDX,1,1,STRIDE_SIZE_IDX,1,1
    // "5k2p1", 5,KERNEL_SIZE_IDX,2,PADDING_SIZE_IDX,1  |
    // "8", 8

    // layer_starting_indexes


    // Options: 
    // 1. Create struct of vectors of int: 
    // 2. Represent in a 1D vector and using a second vector for layer start 
    // 3. Create class

}

}