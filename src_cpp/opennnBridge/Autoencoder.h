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

    double k_, ema_,emad_,ema_normal_,ema_event_;

    double loss_val_;

 public:
    AutoencoderClassifier(double k) : Autoencoder() 
    {
        k_ = k;
        ema_ = 0.0;
        loss_val_=0.0;
        ema_event_=0.0;
        ema_normal_=0.0;
    }

    void ema_update()
    {
        ema_ = ema_ == 0.0 ? this->loss_val_ : this->loss_val_ * alpha + ema_ * (1 - alpha);
    }

    void emad_update()
    {
        emad_ = emad_ == 0.0 ? abs(this->loss_val_-this->ema_) : abs(this->loss_val_-this->ema_) * alpha + emad_ * (1 - alpha);
    }

    void loss_update(double loss_val)
    {
        this->loss_val_=loss_val;
    }

    void ema_event_update()
    {
        this->ema_event_=this->ema_;
    }

    void ema_normal_update()
    {
        this->ema_normal_=this->ema_;
    }

    double getThEvent()
    {
        return this->ema_ + this->k_ * this-> emad_;
    }

    int classification_function(double loss_val)
    {
        loss_update(loss_val);
        ema_update();
        emad_update();
        double thEvent = getThEvent();
        if(thEvent<loss_val)
            ema_event_update();
        else
            ema_normal_update();
        double Th=(this->ema_event_+this->ema_normal_)/2;
        
        return Th<loss_val ? 0 : 1;
    }
    // train of AutoencoderClassifier 

    void train(Tensor2DPtr autoencoder_data, Tensor2DPtr data)
    {
        //std::shared_ptr<OpenNN::NeuralNetwork> neural_network;
        OpenNN::NeuralNetwork *neural_network;
        neural_network = this;
        OpenNN::DataSet data_set;
        OpenNN::TrainingStrategy training_strategy;
        training_strategy.set_data_set_pointer(&data_set);
        training_strategy.set_neural_network_pointer(neural_network);
        training_strategy.set_maximum_epochs_number(1); 
       
      
        Eigen::Tensor<float, 2> train_smaple(1,autoencoder_data->dimension(1));
        Eigen::Tensor<float, 2> predict_smaple(1,data->dimension(1)); 
        int data_num_of_cols = data->dimension(1);
        double loss_val; //MSE error
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::MEAN_SQUARED_ERROR);
        training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
        training_strategy.set_display(0);

        int num_of_samples = autoencoder_data->dimension(0);

        // cout << "autoencider_data: " << *autoencider_data  <<std::endl;
        // cout << "data to train: " << autoencider_data->dimension(0)<< " " << autoencider_data->dimension(1)  <<std::endl;

        cout << "start tarin AutoencoderClassifier total in batch:" << num_of_samples  <<std::endl;
        for(int i = 0 ; i < 1 ; i++){
            train_smaple.chip(0,0) = autoencoder_data.get()->chip(i, 0); //from autoencoder_data tensor get the i's train_smaple (singel sample).
            predict_smaple.chip(0,0) = data.get()->chip(i, 0);
            data_set.set_data(*autoencoder_data); // set data for training.
            data_set.set(autoencoder_data->dimension(1),data_num_of_cols,data_num_of_cols);
            
            training_strategy.perform_training(); // do training on train_smaple (singel sample).

            //autoencider_data.get()[i]
            Eigen::Tensor<float,2> calculate_res;

            calculate_res = neural_network->calculate_outputs(predict_smaple); // calculate the AEC output for predict_smaple 

            // cout << "calculate_res ....." <<std::endl;
            // cout << calculate_res <<std::endl;
            //    Eigen::Tensor<float, 0> MSE_errore = (calculate_res - predict_smaple).pow(2.0).sum().pow(1.0/2.0);  // calculate MSE between the smaple and AEC prediction.
            Eigen::Tensor<float, 0> MSE_errore = (calculate_res - predict_smaple).abs().sum();  // calculate MSE between the smaple and AEC prediction.

            loss_val = MSE_errore(0);
            int RetVal = classification_function(loss_val);

            cout<< "RetVal from classification function: " << RetVal<< endl;
            cout<< "loss_val = " << loss_val << " in sample number " << i <<std::endl;           
           
        }

        
         return;
    }
 // train of AutoencoderClassifier 

    Eigen::Tensor<int, 1> predict(Tensor2DPtr data)
    {
        OpenNN::NeuralNetwork *neural_network;
        neural_network = this;
       
        Eigen::Tensor<float, 2> predict_smaple(1,data->dimension(1)); 

        Eigen::Tensor<int, 1> predictRetTensor(data->dimension(0)); 

        int data_num_of_cols = data->dimension(1);
        double loss_val; //MSE error

        int num_of_samples = data->dimension(0);

        cout << "start tarin AutoencoderClassifier "  <<std::endl;
        for(int i = 0 ; i < num_of_samples ; i++){
        
           predict_smaple.chip(0,0) = data.get()->chip(i, 0);           
 
           Eigen::Tensor<float,2> calculate_res;
           calculate_res = neural_network->calculate_outputs(predict_smaple); // calculate the AEC output for predict_smaple 
            cout << "calculate_res ....." <<std::endl;
            cout << calculate_res <<std::endl;
           Eigen::Tensor<float, 0> MSE_errore = (calculate_res - predict_smaple).pow(2.0).sum().pow(1.0/2.0);  // calculate MSE between the smaple and AEC prediction.
           loss_val = MSE_errore(0);
           int RetVal = classification_function(loss_val);
           predictRetTensor(i) = RetVal;
           cout<< "RetVal from classification function: " << RetVal<< endl;
           cout<< "loss_val = " << loss_val << " in sample number " << i <<std::endl;  
        }

         return predictRetTensor;
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