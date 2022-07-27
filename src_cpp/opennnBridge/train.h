#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>
#include "../opennn/opennn/opennn.h"

#include "definitionsNN.h"

#define TRAINING_STRATEGY_SET_DISPLAY_ON   1
#define TRAINING_STRATEGY_SET_DISPLAY_OFF  0

using namespace OpenNN;             

struct TrainNN {
    long int mid;
    int optimization_method;
    int lose_method;
    double learning_rate;
    std::shared_ptr<Eigen::Tensor<float,2>> data;
    high_resolution_clock::time_point start_time;
    double K_val;

    ErlNifTid tid;
    ErlNifPid pid;
};
 
static void* trainFun(void* arg){ 

         bool flag = true;
         TrainNN* TrainNNptr;

        //  try{
        TrainNNptr = reinterpret_cast<TrainNN*>(arg);
        // cout << "TrainNNptr learning_rate: " << TrainNNptr->learning_rate <<endl; 
        // cout << "TrainNNptr lose_method: " << TrainNNptr->lose_method <<endl; 
        int optimization_method = TrainNNptr->optimization_method ;
        // cout << "TrainNNptr tid: " << TrainNNptr->tid <<endl; 

        //  }
        //  catch(...){
        //    cout << "arg catch" <<std::endl; 
        //  }

         double loss_val;
         ErlNifEnv *env = enif_alloc_env();    
         DataSet data_set;

         
         // Get the singleton instance
         opennnBridgeController *s = s->GetInstance();
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(TrainNNptr->mid);
       
         int modelType = s->getModelType(TrainNNptr->mid);
      
         // pointers to autoencoder data
         std::shared_ptr<Eigen::Tensor<float,2>> autoencoder_data;
         std::shared_ptr<Eigen::Tensor<float,2>> data_temp = std::make_shared<Eigen::Tensor<float,2>>();
    
         // inishalize autoencoder data

         int data_num_of_cols = TrainNNptr->data->dimension(1);
         int NN_input_num = neural_network->get_inputs_number();

         if (modelType == E_AE || modelType == E_AEC){
            Eigen::array<int, 2> bcast({1, 2});  
            autoencoder_data = std::make_shared<Eigen::Tensor<float,2>>(TrainNNptr->data->broadcast(bcast));                 
 
            if(modelType == E_AE){    
              data_set.set_data(*autoencoder_data);
              data_set.set(autoencoder_data->dimension(1),data_num_of_cols,data_num_of_cols);
            }
         }
          
         // inishalize NN data (not autoencoder data)
         else {
            try{
                data_set.set_data(*(TrainNNptr->data));
            }
            catch(...) { 
                flag = false;
                std::cout << "catch set_data " <<std::endl; 
                }
         }


          TrainingStrategy training_strategy;
          if(flag == true) {
            training_strategy.set_neural_network_pointer(neural_network.get()); // Line 80 should be executed before line 81 due to openNN issue
            training_strategy.set_data_set_pointer(&data_set);
        
     

            // set Optimization Method  -------------------------------------------------------------
            try{
            if(optimization_method == E_OM_GRADIENT_DESCENT){
                    training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
                    training_strategy.get_stochastic_gradient_descent_pointer()->set_initial_learning_rate(TrainNNptr->learning_rate);
                }
            else if(optimization_method == E_OM_CONJUGATE_GRADIENT)
                {
                    training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::CONJUGATE_GRADIENT);
                }
            else if(optimization_method == E_OM_QUASI_NEWTON_METHOD)
                {
                    training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD);
                }
            else if(optimization_method == E_OM_LEVENBERG_MARQUARDT_ALGORITHM)
                {
                    training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM);
                }
            else if(optimization_method == E_OM_STOCHASTIC_GRADIENT_DESCENT)
                {
                    training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
                }
            else if(optimization_method == E_OM_ADAPTIVE_MOMENT_ESTIMATION)
                {
                    training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION);
                    training_strategy.get_adaptive_moment_estimation_pointer()->set_initial_learning_rate(TrainNNptr->learning_rate);
                }
                else{
                cout << "TrainNNptr learning_rate: " << TrainNNptr->learning_rate <<endl; 
                cout << "TrainNNptr lose_method: " << TrainNNptr->lose_method <<endl; 
                cout << "TrainNNptr optimization_method: " << TrainNNptr->optimization_method <<endl; 
                cout << "TrainNNptr tid: " << TrainNNptr->tid <<endl;
                    flag = false;
                    cout << "optimization_method not choosen " <<std::endl;
                }
            } //try

            catch(...){
                flag = false;
                cout << "catch - set Optimization Method " <<std::endl;
            }  // end set optimization method ---------------------------------------------------------------     
         
           
           
            // set Loss Method ------------------------------------------------------------------------
            try{
                if(TrainNNptr->lose_method == E_LOSS_METHOD_SUM_SQUARED_ERROR){
                    training_strategy.set_loss_method(TrainingStrategy::LossMethod::SUM_SQUARED_ERROR);
                }
                else if(TrainNNptr->lose_method == E_LOSS_METHOD_MSE)
                {
                    training_strategy.set_loss_method(TrainingStrategy::LossMethod::MEAN_SQUARED_ERROR);
                }
                else if(TrainNNptr->lose_method == E_LOSS_METHOD_NSE)
                {
                    training_strategy.set_loss_method(TrainingStrategy::LossMethod::NORMALIZED_SQUARED_ERROR);
                }
                else if(TrainNNptr->lose_method == E_LOSS_METHOD_MINKOWSKI_ERROR)
                {
                    training_strategy.set_loss_method(TrainingStrategy::LossMethod::MINKOWSKI_ERROR);
                }
                else if(TrainNNptr->lose_method == E_LOSS_METHOD_WSE)
                {
                    training_strategy.set_loss_method(TrainingStrategy::LossMethod::WEIGHTED_SQUARED_ERROR);
                }
                else if(TrainNNptr->lose_method == E_LOSS_METHOD_CEE)
                {
                    training_strategy.set_loss_method(TrainingStrategy::LossMethod::CROSS_ENTROPY_ERROR );   
                }
                else{
                    cout << "lose_method not choosen " <<std::endl;
                }
            } //try

            catch(...){
                cout << "catch - set Loss Method " <<std::endl;
            } 
        } // if(flag == true)
         
         // end set Loss Method ------------------------------------------------------------------------
   

         training_strategy.set_maximum_epochs_number(1); 
         cout << "8848484848484" <<std::endl; 
         training_strategy.set_display(TRAINING_STRATEGY_SET_DISPLAY_OFF);
         
         cout << "9999999999" <<std::endl; 
         // EAC train 
         if(modelType == E_AEC){
            *data_temp = *(TrainNNptr->data);
            int autoencoder_data_num_of_cols = autoencoder_data->dimension(1);
            
            if(data_num_of_cols == 256 && autoencoder_data_num_of_cols == 512 && flag == true){
                std::shared_ptr<AutoencoderClassifier> Autoencoder_Classifier = std::static_pointer_cast<AutoencoderClassifier>(neural_network);
                loss_val = Autoencoder_Classifier->train(autoencoder_data, data_temp);//, neural_network);
            }

            else{
                loss_val = 1;
                cout << "data set error  " <<std::endl; 
                cout << "data_num_of_cols " << data_num_of_cols << std::endl; 
                cout << "autoencoder_data_num_of_cols " << autoencoder_data_num_of_cols << std::endl;
                cout << "flag  " << flag << std::endl;
            }
         }

         else{
 
             if(data_num_of_cols == (NN_input_num+1) && flag == true){
                cout << "1010101010" <<std::endl; 
                TrainingResults  res = training_strategy.perform_training();
               cout << "1212121212" <<std::endl; 

               loss_val = res.get_training_error();
            
            //   cout << "end train " <<std::endl;
                  cout << "888888" <<std::endl; 
         
         // do NN trainig
         //chech the inputs from erlang and neural network architecture ---------------------------------------------------
         //TestingAnalysis testing_analysis(&*neural_network, &data_set);
         //TestingAnalysis testing_analysis;
    /*
                TestingAnalysis testing_analysis;
                //std::shared_ptr<OpenNN::TestingAnalysis> testing_analysis =  std::make_shared<OpenNN::TestingAnalysis>();
                cout << "8181818181" <<std::endl; 
                testing_analysis.set_neural_network_pointer(&*neural_network);
                cout << "8282828282" <<std::endl; 
                testing_analysis.set_data_set_pointer(&data_set);
                cout << "8383838383" <<std::endl; 

            try{ 
               
                Tensor< type, 2 > calculate_errors = testing_analysis.calculate_errors();
            Tensor<type, 1> testing_errors = testing_analysis.calculate_testing_errors();
            //sse - testing_errors[0]
            //mse - testing_errors[1]
            //root mse - testing_errors[2]
            //nse - testing_errors[3]
            ERL_NIF_TERM error = nifpp::makeTensor1D(env, testing_errors);
            if(TrainNNptr->lose_method == E_LOSS_METHOD_SUM_SQUARED_ERROR) loss_val = testing_errors[0]; 
            else if(TrainNNptr->lose_method == E_LOSS_METHOD_MSE)          loss_val = testing_errors[1];  
            else if(TrainNNptr->lose_method == E_LOSS_METHOD_NSE)          loss_val = testing_errors[3]; 
            else loss_val = testing_errors[1];
            loss_val = calculate_errors(0,0);
            
            } 
            catch(...){
                cout << "catch - calculate errors" <<std::endl;
            } 
            */
           }
               else { 
                cout << "data set error  " <<std::endl; 
                cout << "data_num_of_cols " << data_num_of_cols << std::endl; 
                cout << "NN input num " << NN_input_num << std::endl; 
                cout << "flag  " << flag << std::endl;
                }
         }
         cout << "1313131313" <<std::endl; 
         // Stop the timer and calculate the time took for training
         high_resolution_clock::time_point  stop = high_resolution_clock::now();
         auto duration = duration_cast<microseconds>(stop - TrainNNptr->start_time);
          cout << "14141414" <<std::endl; 

         if(isnan(loss_val)  ) {
             loss_val = -1.0;
             cout << "loss val = nan , setting NN weights to random values" <<std::endl;
             neural_network->set_parameters_random();
         }
          cout << "15151515" <<std::endl;
         ERL_NIF_TERM loss_val_term = enif_make_double(env, loss_val);
         ERL_NIF_TERM train_time = enif_make_double(env, duration.count());
        
         ERL_NIF_TERM train_res_and_time = enif_make_tuple(env, 2, loss_val_term,train_time);
          cout << "16161616" <<std::endl;
        if(enif_send(NULL,&(TrainNNptr->pid), env,train_res_and_time)){
            //  printf("enif_send train succeed\n");
        }
        else {
            cout << "loss val:" << loss_val<< endl;
            cout << " train_time:" <<  train_time<< endl;
            printf("enif_send failed\n");
        }
        
         cout << "end train" <<std::endl;
         return 0;
}




/*       FOR DIBUG
         Index layer_num = neural_network->get_layers_number();
         std::cout<< layer_num <<std::endl;
         std::cout<< neural_network->get_layer_pointer(0)->get_type_string() <<std::endl;
         std::cout<< neural_network->get_layer_pointer(0)->get_neurons_number()<<std::endl;
         std::cout<< neural_network->get_layer_pointer(0)->get_inputs_number() <<std::endl;
         std::cout<< neural_network->get_layer_pointer(1)->get_type_string()  <<std::endl;
         std::cout<< neural_network->get_layer_pointer(1)->get_neurons_number() <<std::endl;
         std::cout<< neural_network->get_layer_pointer(2)->get_type_string()  <<std::endl;
         std::cout<< neural_network->get_layer_pointer(2)->get_neurons_number() <<std::endl;
         std::cout<< neural_network->get_layer_pointer(3)->get_type_string()  <<std::endl;
         std::cout<< neural_network->get_layer_pointer(3)->get_neurons_number() <<std::endl;
         cout << "get_output_number " <<std::endl;
         std::cout<< neural_network->get_layer_pointer(3)->get_neurons_number() <<std::endl;
         int si = (neural_network->get_trainable_layers_pointers()).size() ;
         std::cout<< si <<std::endl;
         string type1 = (neural_network->get_trainable_layers_pointers()(1))->get_type_string();
         std::cout<<  type1 <<std::endl;
*/



