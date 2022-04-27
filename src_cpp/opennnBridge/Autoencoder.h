#pragma once 

#include "CustumNN.h"

class Autoencoder : public CustumNN
{
    public: 

    void setAutoencoder()
    {
        this->setCustumNN();

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