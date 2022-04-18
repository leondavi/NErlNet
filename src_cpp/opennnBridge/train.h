#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>
#include "../opennn/opennn/opennn.h"

#include "definitionsNN.h"


using namespace OpenNN;             

struct TrainNN {
    long int mid;
    int optimization_method;
    int lose_method;
    double learning_rate;
    Eigen::Tensor<float,2> data;
    high_resolution_clock::time_point start_time;
    int display;

    ErlNifTid tid;
    ErlNifPid pid;
};
 
static void* trainFun(void* arg){ 
       
         //TrainNN* TrainNNptr = (TrainNN*)arg;
         TrainNN* TrainNNptr = reinterpret_cast<TrainNN*>(arg);
         double loss_val;
         ErlNifEnv *env = enif_alloc_env();         
         DataSet data_set;
         
         // Get the singleton instance
        
         opennnBridgeController *s = s->GetInstance();
            
     

         //cout << "model ID is " <<std::endl;
         //cout << TrainNNptr->mid << std::endl;
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(TrainNNptr->mid);

         int first_layer_size = neural_network->get_layers_neurons_numbers()(0);
         int data_num_of_coloms = TrainNNptr->data.dimension(1);
         //std::cout<< first_layer_size <<std::endl;
         //std::cout<< data_num_of_coloms <<std::endl;

         // check if the neural network is outoencider 
         if (first_layer_size == data_num_of_coloms){
            Eigen::array<int, 2> bcast({1, 2});
            Eigen::Tensor<float, 2> outoencider_data = TrainNNptr->data.broadcast(bcast);     
            data_set.set_data(outoencider_data);
            data_set.set(outoencider_data.dimension(1),data_num_of_coloms,data_num_of_coloms);
         }
         else data_set.set_data(TrainNNptr->data);


         
        
         
         TrainingStrategy training_strategy(&(*(s-> getModelPtr(TrainNNptr->mid))) ,&data_set);
         
        
         // set Optimization Method  -------------------------------------------------------------
        try{
         if(TrainNNptr->optimization_method == E_OM_GRADIENT_DESCENT){
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
             training_strategy.get_stochastic_gradient_descent_pointer()->set_initial_learning_rate(TrainNNptr->learning_rate);
         }
         else if(TrainNNptr->optimization_method == E_OM_CONJUGATE_GRADIENT)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::CONJUGATE_GRADIENT);
         }
         else if(TrainNNptr->optimization_method == E_OM_QUASI_NEWTON_METHOD)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD);
         }
         else if(TrainNNptr->optimization_method == E_OM_LEVENBERG_MARQUARDT_ALGORITHM)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM);
         }
         else if(TrainNNptr->optimization_method == E_OM_STOCHASTIC_GRADIENT_DESCENT)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
         }
         else if(TrainNNptr->optimization_method == E_OM_ADAPTIVE_MOMENT_ESTIMATION)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION);
             training_strategy.get_adaptive_moment_estimation_pointer()->set_initial_learning_rate(TrainNNptr->learning_rate);
         }
         else{
             cout << "optimization_method not choosen " <<std::endl;
         }
        } //try
        catch(...){
           cout << "catch - set Optimization Method " <<std::endl;
        }         
      
         // end set optimization method ---------------------------------------------------------------
         

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
         
         // end set Loss Method ------------------------------------------------------------------------
         
          
         // do NN trainig
         //chech the inputs from erlang and neural network architecture ---------------------------------------------------
         TestingAnalysis testing_analysis(&*neural_network, &data_set);
         
         training_strategy.set_maximum_epochs_number(1); 
         training_strategy.set_display(0);
         //training_strategy.set_display(TrainNNptr->display);
     
        try{   
         training_strategy.perform_training();
        }
        catch(...){
           cout << "catch - do training" <<std::endl;
        }  
     
        try{ 
         Tensor<type, 1> confusion_matrix = testing_analysis.calculate_testing_errors();
         //sse - confusion_matrix[0]
         //mse - confusion_matrix[1]
         //root mse - confusion_matrix[2]
         //nse - confusion_matrix[3]
         ERL_NIF_TERM error = nifpp::makeTensor1D(env, confusion_matrix);
         if(TrainNNptr->lose_method == E_LOSS_METHOD_SUM_SQUARED_ERROR) loss_val = confusion_matrix[0]; 
         else if(TrainNNptr->lose_method == E_LOSS_METHOD_MSE)          loss_val = confusion_matrix[1];  
         else if(TrainNNptr->lose_method == E_LOSS_METHOD_NSE)          loss_val = confusion_matrix[3]; 
         else loss_val = confusion_matrix[1];
        } 
        catch(...){
           cout << "catch - calculate errors" <<std::endl;
        } 
         
         
         // Stop the timer and calculate the time took for training
         high_resolution_clock::time_point  stop = high_resolution_clock::now();
         auto duration = duration_cast<microseconds>(stop - TrainNNptr->start_time);
         if(isnan(loss_val)  ) {
             loss_val = -1.0;
             cout << "loss val = nan , please stop the raining and try another hiper parameters" <<std::endl;
         }
         ERL_NIF_TERM loss_val_term = enif_make_double(env, loss_val);
         ERL_NIF_TERM train_time = enif_make_double(env, duration.count());
         
         //cout << duration.count() <<std::endl;
        
         //ERL_NIF_TERM train_res_and_time = enif_make_tuple(env, 2, loss_val_term,enif_make_double(env, duration.count()));
         ERL_NIF_TERM train_res_and_time = enif_make_tuple(env, 2, loss_val_term,train_time);
              
         
         if(enif_send(NULL,&(TrainNNptr->pid), env,loss_val_term)){
             printf("enif_send train succeed\n");
         }
         else printf("enif_send failed\n");
         //delete TrainNNptr;
         return 0;
         //return enif_make_string(env, "end TRAIN mode", ERL_NIF_LATIN1);
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


/*
         //get weitghts test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         Tensor< float, 1 > parameters1 = neural_network->get_parameters();
         std::cout << parameters1 << std::endl;
         std::cout << "bbbb" << std::endl; 
         //end 
         */

        //cout << TrainNNptr->data <<std::endl;

                //Tensor<float, 2> confusion_matrix = testing_analysis.calculate_percentage_error_data();
        //printf("calculate_testing_errors\n" );
         //std::cout << testing_analysis.calculate_percentage_error_data() <<std::endl;
         //cout << mean_squared_error.calculate_error() <<std::endl; 
         
         //mean_squared_error.set(&*neural_network, &data_set);
         //cout << "mean_squared_error" <<std::endl;
         //cout << mean_squared_error.get_display() <<std::endl;

         /*
         ERL_NIF_TERM error = nifpp::makeTensor2D(env, confusion_matrix);

         if(enif_send(NULL,&(TrainNNptr->pid), env,error)){
             printf("enif_send succeed\n");
         }
         else printf("enif_send failed\n");
         */
