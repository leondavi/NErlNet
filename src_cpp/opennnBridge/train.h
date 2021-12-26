#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>
//#include <eigen3/Eigen/Core>
#include "../opennn/opennn/opennn.h"

#include "definitionsNN.h"

using namespace OpenNN;             

struct TrainNN {

    long int mid;
    int optimization_method;
    int lose_method;
    Eigen::Tensor<float,2> data;

    ErlNifTid tid;
    ErlNifPid pid;
};
 
static void* trainFun(void* arg){ 
         printf("start train\n");
         TrainNN* TrainNNptr = (TrainNN*)arg;
         int loss_val;
         ErlNifEnv *env = enif_alloc_env();
         printf("data_set \n");
         //cout << TrainNNptr->data <<std::endl;
         DataSet data_set;
         data_set.set_data(TrainNNptr->data);
         printf("end data_set \n");
         //MeanSquaredError mean_squared_error;
         
         
         // Get the singleton instance
         opennnBridgeController *s = s->GetInstance();
         printf("Get the model from the singleton \n");
         // Get the model from the singleton
         cout << "model ID is " <<std::endl;
         cout << TrainNNptr->mid << std::endl;
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(TrainNNptr->mid);
         //NeuralNetwork neural_network = *(s-> getModelPtr(mid));


         printf("end Get the model from the singleton \n");
         

         //cout << neural_network->get_layers_number() <<std::endl;
         //TrainingStrategy training_strategy(&neural_network ,&data_set);

         // ask david
         TrainingStrategy training_strategy(&(*(s-> getModelPtr(TrainNNptr->mid))) ,&data_set);

         
         printf("set Optimization Method\n");
         // set Optimization Method  -------------------------------------------------------------
         if(TrainNNptr->optimization_method == E_OM_GRADIENT_DESCENT){
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::GRADIENT_DESCENT);
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
             
         }
         else{
             cout << "optimization_method not choosen " <<std::endl;
         }
         printf("end set Optimization Method\n");
         // end set optimization method ---------------------------------------------------------------
         


         // set Loss Method ------------------------------------------------------------------------
         printf("set set Loss Method \n");
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
         printf("end set Loss Method \n");
         // end set Loss Method ------------------------------------------------------------------------
         
         printf("ckech NN parameters\n");
         Index layer_num = neural_network->get_layers_number();
         std::cout<< "your layer_num : " <<std::endl;
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

  
         //MeanSquaredError error = MeanSquaredError(&*neural_network , &data_set);//.calculate_error();
          
         // do NN trainig
         //chech the inputs from erlang and neural network architecture ---------------------------------------------------
         TestingAnalysis testing_analysis(&*neural_network, &data_set);
         


         //mean_squared_error.set(&*neural_network, &data_set);
         //cout << "mean_squared_error" <<std::endl;
         //cout << mean_squared_error.get_display() <<std::endl;
         printf( "befor training\n" );
         training_strategy.perform_training();
         printf ("after training\n" ); 
         //Tensor<float, 2> confusion_matrix = testing_analysis.calculate_percentage_error_data();
         printf("calculate_testing_errors\n" );
         //std::cout << testing_analysis.calculate_percentage_error_data() <<std::endl;
         //cout << mean_squared_error.calculate_error() <<std::endl; 
         /*
         //get weitghts test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         Tensor< float, 1 > parameters1 = neural_network->get_parameters();
         std::cout << parameters1 << std::endl;
         std::cout << "bbbb" << std::endl; 
         //end 
         */

         loss_val = 0;
         /*
         ERL_NIF_TERM error = nifpp::makeTensor2D(env, confusion_matrix);

         if(enif_send(NULL,&(TrainNNptr->pid), env,error)){
             printf("enif_send succeed\n");
         }
         else printf("enif_send failed\n");
         */

         ERL_NIF_TERM loss_val_term = enif_make_int(env, loss_val);
         if(enif_send(NULL,&(TrainNNptr->pid), env,loss_val_term)){
             printf("enif_send succeed\n");
         }
         else printf("enif_send failed\n");
         
         delete TrainNNptr;
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
