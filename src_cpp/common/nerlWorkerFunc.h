#pragma once

#include <memory>
#include <unordered_map>
#include <Logger.h>
#include "utilities.h"
#include "worker_definitions_ag.h"
//TODO:i probably need to move NERLPLANNER_INPUT to utilities.h
#define NERLPLANNER_INPUT_KERNEL_CHAR 'k'
#define NERLPLANNER_INPUT_STRIDE_CHAR 's'
#define NERLPLANNER_SIZE_DIMENSION_SEP "x"
#define NERLPLANNER_INPUT_PADDING_CHAR 'p'



#define SIMPLE_PARSING -1
#define COMPLEX_PARSING -2


namespace nerlnet
{

typedef struct LayerSizingParams
{
 enum {KERNEL_SIZE = -1, PADDING_SIZE = -2,PADDING_SIZE_VALID = -3, STRIDE_SIZE = -4, POOLING_SIZE= -5};
 int dimx = 1;
 int dimy = 1;
 int dimz = 1; 
 std::vector<int> _ext_params; 

 int get_maxdim() { return (dimz > 1 ? 3 : dimy > 1 ? 2 : 1);} // return the maximum dimension of the param; 

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
                                    case SIMPLE_PARSING:
                                    {
                                        out_layer_sizes_params[i].dimx = std::stoi(layer_sizes_strs_vec[i]);
                                        break;
                                    }
                                    case COMPLEX_PARSING:
                                    {
                                        LogInfo("Complex parsing");
                                        std::unordered_map<char, std::string> params;
                                        std::regex rgx("(\\d+)x(\\d+)(x(\\d+))?");
                                        std::smatch matches; //this matches variable is for the layer size
                                        std::smatch param_match; // this matches variable is for the rest of the string
                                        std::unordered_map<char, int> param_codes = {
                                            {'k', -1},
                                            {'p', -2},
                                            {'s', -3}
                                        };
                                        std::regex rgx_rest("([ksp])(\\d+x\\d+(x\\d+)?)"); //search for k, s or p followed by a number and then x and then a number

                                        std::string::const_iterator searchStart(layer_sizes_strs_vec[i].cbegin());
                                        while (std::regex_search(searchStart, layer_sizes_strs_vec[i].cend(), param_match, rgx))
                                        {
                                            char param_char = param_match[1].str()[0]; //the first character of the match
                                            std::string dimensions_str = param_match[2].str();//the second part of the match (the dimensions)

                                            // Convert the parameter and dimensions to the desired format and add them to _ext_params
                                            out_layer_sizes_params[i]._ext_params.push_back(param_codes[param_char]);
                                            std::istringstream dimensions_stream(dimensions_str);
                                            std::string dimension;
                                            while (std::getline(dimensions_stream, dimension, 'x'))
                                            {
                                                out_layer_sizes_params[i]._ext_params.push_back(std::stoi(dimension));
                                            }

                                            searchStart = param_match.suffix().first;
                                        }
                                        break;
                                    }

                                    default:
                                        LogError("Error parsing layer size string");
                                        break;
                                }
                            }
                        }
                    } // Closing brace for namespace nerlnet
                // Closing brace for namespace nerlnet



                
  
            
  
    

    // "5x5k2x2p1x1s1", 5,5,KERNEL_SIZE_IDX,2,2,PADDING_SIZE_IDX,1,1,STRIDE_SIZE_IDX,1,1
    // "5k2p1", 5,KERNEL_SIZE_IDX,2,PADDING_SIZE_IDX,1  |
    // "8", 8

    // layer_starting_indexes


    // Options: 
    // 1. Create struct of vectors of int: 
    // 2. Represent in a 1D vector and using a second vector for layer start 
    // 3. Create class

//