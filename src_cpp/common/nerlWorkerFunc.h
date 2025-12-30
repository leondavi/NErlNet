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
enum {KERNEL_SIZE = -1, PADDING_SIZE = -2,STRIDE_SIZE = -3 ,POOLING_SIZE= -4 , IS_VALID = -5};
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
  while (!param_extracted && i < _ext_params.size()){
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
 if(!param_extracted){
     res.push_back(0);
     res.push_back(0);
 }
 return res;
 }
} LayerSizingParams_t;

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
                            std::unordered_map<char, std::string> params;
                            std::regex rgx_dim("[0-9][^kstpx]*");
                            std::smatch matches; //this matches variable is for the layer size
                            std::smatch param_match; // this matches variable is for the rest of the string
                            std::smatch dim_match; // this matches variable is for the dimensions
                            std::unordered_map<char, int> param_codes = {
                                {'k', -1},
                                {'p', -2},
                                {'s', -3},
                                {'t',-5}
                            };
                            std::string::const_iterator searchStartDim(layer_sizes_strs_vec[i].cbegin());
                            for (size_t k = 0; k < 3; k++){
                                 std::regex_search(searchStartDim, layer_sizes_strs_vec[i].cend(), dim_match, rgx_dim);
                                    if(k == 0){
                                        out_layer_sizes_params[i].dimx = std::stoi(dim_match[0]);
                                    }else if(k == 1){
                                        out_layer_sizes_params[i].dimy = std::stoi(dim_match[0]);
                                    }else{                               
                                        out_layer_sizes_params[i].dimz = std::stoi(dim_match[0]);
                                    }
                                 searchStartDim = dim_match.suffix().first;
                            }
                            std::regex rgx_rest("[kspt]([0-9]*x?[0-9]*)*"); //search for k, s or p followed by a number and then x and then a number
                            std::string::const_iterator searchStart(layer_sizes_strs_vec[i].cbegin());
                            while (std::regex_search(searchStart, layer_sizes_strs_vec[i].cend(), param_match, rgx_rest))
                                {
                                    char param_char = param_match[0].str()[0]; //the first character of the match
                                    std::string dimensions_str = param_match.str();//the second part of the match (the dimensions)
                                    std::string dimensions_str_sub = dimensions_str.substr(1,-1);
                                    // Convert the parameter and dimensions to the desired format and add them to _ext_params
                                    out_layer_sizes_params[i]._ext_params.push_back(param_codes[param_char]);
                                    std::istringstream dimensions_stream(dimensions_str_sub);
                                    std::string dimension;
                                    while (std::getline(dimensions_stream, dimension, 'x'))
                                    {
                                        out_layer_sizes_params[i]._ext_params.push_back(std::stoi(dimension));
                                    }
                                    if(dimensions_str_sub.length() == 1 && param_char!='t') out_layer_sizes_params[i]._ext_params.push_back(std::stoi(dimension));
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