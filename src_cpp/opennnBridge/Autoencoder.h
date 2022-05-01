#pragma once 

#include "CustumNN.h"

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



    void prepare_data(Tensor2DPtr data ,Tensor2DPtr & autoencoder_data)
    {
        Eigen::array<int, 2> bcast({1, 2});
        *autoencoder_data = (data->broadcast(bcast));    // this copy the tensor 
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