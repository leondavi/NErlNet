#pragma once 

#include "CustumNN.h"

class Autoencoder : public CustumNN
{
    public: 

    Autoencoder() : CustumNN()
    {

    }

    void setAutoencoder(Tensor1DPtr neural_network_architecture , Tensor1DPtr layer_types, 
                          Tensor1DPtr activations_functions)
    {
        this->setCustumNN(neural_network_architecture, layer_types, activations_functions);
    }

};



class AutoencoderClassifier : public Autoencoder
{
 
    public:

    const alpha = 0.7;

    double ema_;
    double emad_; // variance
    double k_;

    Autoencoder_Classifier(double k) : Autoencoder() 
    {
        k_ = k
        ema = 0.0;
    }

    void ema_update(double loss_val)
    {
        ema = ema == 0.0 ? loss_val : loss_val * alpha + ema * (1 - alpha);
    }

    bool classification_function()
    {
        return false;
    }
    // train of AutoencoderClassifier 
    /*
    void train(Tensor2DPtr outoencider_data)
    {
        std::shared_ptr<OpenNN::NeuralNetwork> neural_network;
        neural_network = this;
        OpenNN::DataSet data_set;
        OpenNN::TrainingStrategy training_strategy;
        int num_of_samples = outoencider_data.dimension(0);
        for(int i = 0 ; i < num_of_samples ; i++){
           data_set.set_data((*outoencider_data)(i));
           training_strategy.set(&*neural_network ,&data_set);
        
           
        }


      
    }
  */
/*
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
*/
};



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