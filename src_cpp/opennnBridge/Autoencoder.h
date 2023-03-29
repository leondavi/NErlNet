/*****************************************************
 * Authors: Evgeni Andrachnic
 * 29/10/2021
 * 
 * @copyright Copyright (c) 2021 Nerlnet
 *****************************************************/ 

#pragma once 

#include "CustumNN.h"

class Autoencoder : public CustumNN
{
    public: 

    Autoencoder() : CustumNN()
    {

    }

    void setAutoencoder(iTensor1DPtr neural_network_architecture , iTensor1DPtr layer_types, 
                          iTensor1DPtr activations_functions)
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
        ema_ = ema_ == 0.0 ? this->loss_val_ : (this->loss_val_ * alpha )+ (ema_ * (1 - alpha));
    }

    void emad_update()
    {
        emad_ = emad_ == 0.0 ? abs(this->loss_val_-this->ema_) : (abs(this->loss_val_-this->ema_) * alpha) + (emad_ * (1 - alpha));
        
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
        
        return Th<loss_val ? 1 : 0;
    }

    // train of AutoencoderClassifier 
    float train(fTensor2DPtr autoencoder_data, fTensor2DPtr data)//, std::shared_ptr<OpenNN::NeuralNetwork> neural_network)
    {
      
        bool eac_flag = true;
        //std::shared_ptr<OpenNN::NeuralNetwork> neural_network(this);
        OpenNN::NeuralNetwork *neural_network;
        neural_network = this;
        OpenNN::DataSet data_set;
        OpenNN::TrainingStrategy training_strategy;
        training_strategy.set_neural_network_pointer(neural_network);
        training_strategy.set_data_set_pointer(&data_set);
        training_strategy.set_maximum_epochs_number(1); 
       
      
        int data_num_of_cols = data->dimension(1);
        float loss_val; 
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::MEAN_SQUARED_ERROR);
        training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
        //batch_samples_number
        training_strategy.set_display(1);

    
        int num_of_aec_cols = autoencoder_data->dimension(1);
       

            try{
            data_set.set_data(*autoencoder_data); // set data for training.
            }
            catch(...){
                    eac_flag = false;
                    std::cout << "catch AEC set_data " <<std::endl; 
            }
             
            data_set.set(autoencoder_data->dimension(1),data_num_of_cols,data_num_of_cols);
          
            if(eac_flag == true && num_of_aec_cols == 512){
               TrainingResults  res = training_strategy.perform_training(); // do training on train_smaple (singel sample).
                cout  << "****train error: " << res.get_training_error() << endl;
                TestingAnalysis testing_analysis(&*neural_network, &data_set);
                Eigen::Tensor<float,2> output = neural_network->calculate_outputs(*data); // calculate the AEC output for predict_smaple 
                cout<< "output(0) "<< output(0) << endl;
                Eigen::Tensor<float,2> loss = (output - *data).abs();

                for(int j = 0; j < loss.dimension(0); j++){
                    float sum = 0;
                for(int i = 0; i < loss.dimension(1); i++){
                    sum += loss(j,i);
                }
                    cout<<"sum "<<j<<" : " << sum<<endl;
                    loss_val = res.get_training_error();
                    int RetVal = classification_function(loss_val);
                    cout<< "RetVal from classification function: " << RetVal<< endl;

                    // Tal func here
                }
                    cout<< " "<< endl;

                cout << "loss_val " << loss_val <<  endl;
           
            }
            else{
                cout << "eac_flag " << eac_flag << endl;
                cout << "num_of_aec_cols " << num_of_aec_cols << endl;
            }
           
         return loss_val;
    }
 // train of AutoencoderClassifier 


//===================
//AEC Predict:
//===================

    iTensor1DPtr predict(fTensor2DPtr data)
    {
        OpenNN::NeuralNetwork *neural_network;
        neural_network = this;
       
        Eigen::Tensor<float, 2> predict_smaple(1,data->dimension(1)); 

        iTensor1DPtr predictRetTensor = std::make_shared<iTensor1D>(data->dimension(0)); 

        int data_num_of_cols = data->dimension(1);
        double loss_val; //MSE error

        int num_of_samples = data->dimension(0);

        int predRet[num_of_samples]; 
        float lossRet[num_of_samples]; 


      
        for(int i = 0 ; i < num_of_samples ; i++){
        
           predict_smaple.chip(0,0) = data.get()->chip(i, 0);           
 
           Eigen::Tensor<float,2> calculate_res;
           calculate_res = neural_network->calculate_outputs(predict_smaple); // calculate the AEC output for predict_smaple 
      
           Eigen::Tensor<float, 0> MSE_errore = (calculate_res - predict_smaple).abs().sum();  // calculate MSE between the smaple and AEC prediction.
           loss_val = MSE_errore(0);
           Eigen::Tensor<float, 2> mseError = (calculate_res - predict_smaple).abs();  // calculate MSE between the smaple and AEC prediction.
          // cout<< "mseError = " << mseError <<endl ;  

           int RetVal = classification_function(loss_val);
           predRet[i]=RetVal;
           lossRet[i]=loss_val;
           (*predictRetTensor)[i] = RetVal;
 
        }

         return predictRetTensor;
    }


};


