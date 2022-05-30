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
 
   

    const double alpha = 0.7;

    double ema_;
    double emad_; // variance
    double k_;

 public:
    AutoencoderClassifier(double k) : Autoencoder() 
    {
        k_ = k;
        ema_ = 0.0;
    }

    void ema_update(double loss_val)
    {
        ema_ = ema_ == 0.0 ? loss_val : loss_val * alpha + ema_ * (1 - alpha);
    }

    bool classification_function()
    {
        return false;
    }
    // train of AutoencoderClassifier 

    void train(Tensor2DPtr autoencider_data, Tensor2DPtr data)
    {
        //std::shared_ptr<OpenNN::NeuralNetwork> neural_network;
        OpenNN::NeuralNetwork *neural_network;
        neural_network = this;
        OpenNN::DataSet data_set;
        OpenNN::TrainingStrategy training_strategy;
        training_strategy.set_data_set_pointer(&data_set);
        training_strategy.set_neural_network_pointer(neural_network);
        training_strategy.set_maximum_epochs_number(1); 
       
      
        Eigen::Tensor<float, 2> train_smaple(1,autoencider_data->dimension(1));
        Eigen::Tensor<float, 2> predict_smaple(1,data->dimension(1)); 
        int data_num_of_cols = data->dimension(1);
        double loss_val; //MSE error
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::CROSS_ENTROPY_ERROR);
        training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
        training_strategy.set_display(0);

        int num_of_samples = autoencider_data->dimension(0);

        cout << "start tarin AutoencoderClassifier "  <<std::endl;
        for(int i = 0 ; i < num_of_samples ; i++){
        
           train_smaple.chip(0,0) = autoencider_data.get()->chip(i, 0); //from autoencider_data tensor get the i's train_smaple (singel sample).
            
           predict_smaple.chip(0,0) = data.get()->chip(i, 0);
            
           data_set.set_data(train_smaple); // set data for training.
            
           data_set.set(autoencider_data->dimension(1),data_num_of_cols,data_num_of_cols);

           training_strategy.perform_training(); // do training on train_smaple (singel sample).
           
          //autoencider_data.get()[i]
           Eigen::Tensor<float,2> calculate_res;
           calculate_res = neural_network->calculate_outputs(predict_smaple); // calculate the AEC output for predict_smaple 
            cout << "calculate_res ....." <<std::endl;
            cout << calculate_res <<std::endl;
           Eigen::Tensor<float, 0> MSE_errore = (calculate_res - predict_smaple).pow(2.0).sum().pow(1.0/2.0);  // calculate MSE between the smaple and AEC prediction.
           loss_val = MSE_errore(0);
           
           cout<< "loss_val = " << loss_val << " in sample number " << i <<std::endl;
           ema_update(loss_val);
           
           
        }

        
         return;
    }

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