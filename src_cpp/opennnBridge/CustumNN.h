#pragma once 

#include "../opennn/opennn/opennn.h"
#include "definitionsNN.h"

class CustumNN : public opennn::NeuralNetwork
{

public:

    CustumNN() : opennn::NeuralNetwork()
    {
        
    }

    void setCustumNN(iTensor1DPtr neural_network_architecture , iTensor1DPtr layer_types, 
                          iTensor1DPtr activations_functions ){
        
       std::cout << "start CustumNN" << std::endl; 
       std::cout << neural_network_architecture->size() << std::endl; 
    
       //std::shared_ptr<opennn::NeuralNetwork> neural_network = std::make_shared<opennn::NeuralNetwork>();    

             //  creat neural networl layers
            for(int i = 0 ; i < neural_network_architecture->size() ; i++){
                 
                if ((*layer_types)[i] == E_LAYER_TYPE_SCALING){
                    opennn::ScalingLayer *L = new opennn::ScalingLayer; // TODO: remove all news
                    L->set((*neural_network_architecture)(i));
                    opennn::NeuralNetwork::add_layer(L);
                }
        
                if ((*layer_types)[i] == E_LAYER_TYPE_PERCEPTRON){
                    opennn::PerceptronLayer *L = new opennn::PerceptronLayer((*neural_network_architecture)(i-1), (*neural_network_architecture)(i));
                    opennn::NeuralNetwork::add_layer(L);
                }
            
                if ((*layer_types)[i] == E_LAYER_TYPE_PROBABILISTIC){
                    opennn::ProbabilisticLayer *L = new opennn::ProbabilisticLayer((*neural_network_architecture)(i-1), (*neural_network_architecture)(i));;
                    opennn::NeuralNetwork::add_layer(L);
                }

                if ((*layer_types)[i] == E_LAYER_TYPE_UNSCALING){
                    opennn::UnscalingLayer *L = new opennn::UnscalingLayer;
                    L->set((*neural_network_architecture)(i));
                    opennn::NeuralNetwork::add_layer(L);
                }
                //E_LAYER_TYPE_UNSCALING

                std::cout << "set layer " << i << "to be size " << (*neural_network_architecture)(i) << std::endl;
                // std::cout << "layer types" << std::endl;
                // if((*layer_types)[i] != 1){
                //     std::cout<< opennn::NeuralNetwork::get_layer_pointer(i)->get_type_string() <<std::endl;
                //     std::cout<< opennn::NeuralNetwork::get_layer_pointer(i)->get_neurons_number()<<std::endl;
                // }
            }
                 
                std::cout << "layers number: "<< opennn::NeuralNetwork::get_layers_number() << std::endl; 
                std::cout << "end CustumNN" << std::endl; 


    };

    
    
private:
   

    


};
