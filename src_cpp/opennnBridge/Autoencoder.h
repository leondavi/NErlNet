#pragma once 

#include "CustumNN.h"
using Tensor2DPtr = std::shared_ptr<Tensor<float,2>>;
class Autoencoder : public CustumNN
{
    public: 

    void setAutoencoder(Tensor1DPtr neural_network_architecture , Tensor1DPtr layer_types, 
                          Tensor1DPtr activations_functions)
    {
        this->setCustumNN(neural_network_architecture, layer_types, activations_functions);

        // check for AE constraints:
        // Symetric layers 
    }

    void train()
    {
        // input layer == output layer
    }

    void predict()
    {
        //input layer == output layer
        // vector of prediction 
    }
    
};

/*

class AutoencoderClassifier : public Autoencoder
{
    public:

    void train()
    {
        train() of AE 
        for each sample calculate Error (MSE)

        returns anomaly classifier result 

        return vector of labels - doesn't impact AE train
    }

    void AnomalyClassifier(mse)
    {
        // Based on David's thesis 

        returns classification based on mse
    }

    void predict()
    {
        // different predict 
        AnomalyClassifier - return vecotr of labels
    }
};

*/

int EAC_predic(Tensor2DPtr EA_input, Tensor2DPtr EA_output){
    int EAC_prediction;
    // TAL
    //return EAC_prediction;
    return 1;
}

int EAC_train(Tensor2DPtr EA_input, Tensor2DPtr EA_output ){
    int EAC_prediction;
    // TAL
    //return EAC_prediction;
    return 1;
}