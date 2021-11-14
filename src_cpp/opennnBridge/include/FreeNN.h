#pragma once 

#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"
#include "bridgeController.h"

class FreeNN 
{
public:
 	FreeNN() {};
 	FreeNN(OpenNN::NeuralNetwork neuralNetwork , Eigen::Tensor<int,1> layersSizes, Eigen::Tensor<int,1> layersTypes,
           Eigen::Tensor<int,1> activationFunctions);
 	
    
    OpenNN::NeuralNetwork GetNeuralNetwork() { return _neuralNetwork; };
    Eigen::Tensor<int,1> GetLayersSizes() { return _layersSizes; };
    Eigen::Tensor<int,1> GetLayersTypes() { return _layersTypes; };
    Eigen::Tensor<int,1> GetAcvtivationFunctions() { return _activationFunctions; };
    
    

    void setNeuralNetwork(Eigen::Tensor<int,1>  Sizes , Eigen::Tensor<int,1>  Types , 
    Eigen::Tensor<int,1>  Functions , OpenNN::NeuralNetwork  Network ){
        
       // this->_activationList = x;

    };

    void setAcvtivationList(Eigen::Tensor<int,1>  x){this->_activationFunctions = x; };
    void setLayersSizes(Eigen::Tensor<int,1>  x){this->_layersSizes = x; };
    void setLayersTypes(Eigen::Tensor<int,1>  x){this->_layersTypes = x; };
    
    
    
private:
   
    OpenNN::NeuralNetwork _neuralNetwork; 
    Eigen::Tensor<int,1> _layersSizes;
    Eigen::Tensor<int,1> _layersTypes;
    Eigen::Tensor<int,1> _activationFunctions;
    


};
