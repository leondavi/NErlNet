#pragma once 

//#include <eigen3/Eigen/Core>
#include "../opennn/opennn/opennn.h"
#include "definitionsNN.h"


class CustumNN : public OpenNN::NeuralNetwork
{

public:

    CustumNN() : OpenNN::NeuralNetwork()
    {
        
    }

    

    void setCustumNN(Tensor1DPtr neural_network_architecture , Tensor1DPtr layer_types, 
                          Tensor1DPtr activations_functions ){
        
       std::cout << "start CustumNN" << std::endl; 
       std::cout << neural_network_architecture->size() << std::endl; 
    
       //std::shared_ptr<OpenNN::NeuralNetwork> neural_network = std::make_shared<OpenNN::NeuralNetwork>();    

             //  creat neural networl layers
             for(int i = 0 ; i < neural_network_architecture->size() ; i++){
              
                
                 
                 if ((*layer_types)[i] == E_LAYER_TYPE_SCALING){
                     OpenNN::ScalingLayer *L = new OpenNN::ScalingLayer; 
                     L->set((*neural_network_architecture)(i));
                     OpenNN::NeuralNetwork::add_layer(L);
          
                 }
          
                 if ((*layer_types)[i] == E_LAYER_TYPE_PERCEPTRON){
                     OpenNN::PerceptronLayer *L = new OpenNN::PerceptronLayer((*neural_network_architecture)(i-1), (*neural_network_architecture)(i));
                     OpenNN::NeuralNetwork::add_layer(L);
                 }

                
                 if ((*layer_types)[i] == E_LAYER_TYPE_PROBABILISTIC){
                     OpenNN::ProbabilisticLayer *L = new OpenNN::ProbabilisticLayer((*neural_network_architecture)(i-1), (*neural_network_architecture)(i));;
                     OpenNN::NeuralNetwork::add_layer(L);
                 }

                  if ((*layer_types)[i] == E_LAYER_TYPE_UNSCALING){
                     OpenNN::UnscalingLayer *L = new OpenNN::UnscalingLayer;
                     L->set((*neural_network_architecture)(i));
                     OpenNN::NeuralNetwork::add_layer(L);
                 }
                 //E_LAYER_TYPE_UNSCALING


                 std::cout << "layer types" << std::endl;
                 std::cout<< OpenNN::NeuralNetwork::get_layer_pointer(i)->get_type_string() <<std::endl;
                 std::cout<< OpenNN::NeuralNetwork::get_layer_pointer(i)->get_neurons_number()<<std::endl;
             }
                 
                 std::cout << "layers number: "<< OpenNN::NeuralNetwork::get_layers_number() << std::endl; 
                 std::cout << "end CustumNN" << std::endl; 


    };

    
    
private:
   

    


};
