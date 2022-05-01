#pragma once 

//#include <eigen3/Eigen/Core>
#include "../opennn/opennn/opennn.h"
#include "definitionsNN.h"

using Tensor1DPtr = std::shared_ptr<Tensor<Index,1>>;
class CustumNN : public OpenNN::NeuralNetwork
{
public:
 	//CustumNN() {};
 	//CustumNN(); // OpenNN::NeuralNetwork neuralNetwork , Eigen::Tensor<int,1> layersSizes, Eigen::Tensor<int,1> layersTypes,
               //Eigen::Tensor<int,1> activationFunctions);
 	
    
   // OpenNN::NeuralNetwork GetNeuralNetwork() { return _neuralNetwork; };
   // Eigen::Tensor<int,1> GetLayersSizes() { return _layersSizes; };
   // Eigen::Tensor<int,1> GetLayersTypes() { return _layersTypes; };
   // Eigen::Tensor<int,1> GetAcvtivationFunctions() { return _activationFunctions; };
    
    

    void setCustumNN(Tensor1DPtr neural_network_architecture , Tensor1DPtr layer_types, 
                          Tensor1DPtr activations_functions ){
        
       // this->_activationList = x;
       std::cout << "start CustumNN" << std::endl; 
       //std::shared_ptr<OpenNN::NeuralNetwork> neural_network = std::make_shared<OpenNN::NeuralNetwork>();    

             //  creat neural networl layers
             for(int i = 0 ; i < neural_network_architecture->size() ; i++){
                 
                 if ((*layer_types)[i] == E_LAYER_TYPE_SCALING){
                     std::shared_ptr<OpenNN::ScalingLayer> L = std::make_shared<OpenNN::ScalingLayer>(); 
                     //std::shared_ptr<OpenNN::ScalingLayer> L(new OpenNN::ScalingLayer()); //not work
                     L->set((*neural_network_architecture)[i]);
                     //neural_network->add_layer(L);
                     OpenNN::NeuralNetwork::add_layer(L.get());
                      
                 }

                 if ((*layer_types)[i] == E_LAYER_TYPE_PERCEPTRON){
                      std::shared_ptr<OpenNN::PerceptronLayer> L = std::make_shared<OpenNN::PerceptronLayer>();
                     //std::shared_ptr<OpenNN::PerceptronLayer> L = std::make_shared<OpenNN::PerceptronLayer>(); // not work
                     L->set_neurons_number((*neural_network_architecture)[i]);
                     //neural_network->add_layer(L); 
                     OpenNN::NeuralNetwork::add_layer(L.get());
                 }

                 if ((*layer_types)[i] == E_LAYER_TYPE_PROBABILISTIC){
                     std::shared_ptr<OpenNN::ProbabilisticLayer> L = std::make_shared<OpenNN::ProbabilisticLayer>();
                     //std::shared_ptr<OpenNN::ProbabilisticLayer> L = std::make_shared<OpenNN::ProbabilisticLayer>(); //not work
                     L->set_neurons_number((*neural_network_architecture)[i]);
                     //neural_network->add_layer(L); 
                     OpenNN::NeuralNetwork::add_layer(L.get());
                 }

                 //this = neural_network;
                 std::cout << OpenNN::NeuralNetwork::get_layers_number() << std::endl; 
                 std::cout << "end CustumNN" << std::endl; 
             }


    };

    // void setAcvtivationList(Eigen::Tensor<int,1>  x){this->_activationFunctions = x; };
    // void setLayersSizes(Eigen::Tensor<int,1>  x){this->_layersSizes = x; };
    // void setLayersTypes(Eigen::Tensor<int,1>  x){this->_layersTypes = x; };
    
    
    
private:
   
   // OpenNN::NeuralNetwork _neuralNetwork; 
   // Eigen::Tensor<int,1> _layersSizes;
   // Eigen::Tensor<int,1> _layersTypes;
   // Eigen::Tensor<int,1> _activationFunctions;
    


};
