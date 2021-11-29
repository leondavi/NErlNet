#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>
#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"

using namespace OpenNN;

enum LossMethod {Sum_Squared_Error = 1, Mean_Squared_Error = 2, Normalized_Squared_Error = 3 ,
                         Minkowski_Error = 4, Weighted_Squared_Error = 5 , Cross_Entropy_Error = 6};

enum OptimizationMethod {GRADIENT__DESCENT = 1, CONJUGATE__GRADIENT = 2, QUASI__NEWTON_METHOD = 3 ,
                         LEVENBERG__MARQUARDT_ALGORITHM = 4, STOCHASTIC__GRADIENT_DESCENT = 5 , ADAPTIVE__MOMENT_ESTIMATION = 6};
               

struct TrainNN {

    long int mid;
    int optimization_method;
    int lose_method;
    Eigen::Tensor<float,2> data;

    ErlNifTid tid;
    ErlNifPid pid;
};

static void* trainFun(void* arg){ 
         TrainNN* TrainNNptr = (TrainNN*)arg;
         double loss_val;
         ErlNifEnv *env = enif_alloc_env();

         DataSet data_set;
         data_set.set_data(TrainNNptr->data);
         
         // Get the singleton instance
         opennnBridgeController *s = s->GetInstance();
         
         // Get the model from the singleton
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(TrainNNptr->mid);
         
         //NeuralNetwork neural_network = *(s-> getModelPtr(mid));

         /*/ get weights test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         Tensor< float, 1 > parameters = neural_network->get_parameters();
         std::cout << parameters << std::endl;
         std::cout << "bbbb" << std::endl; 
         // end */ 
         
         cout << neural_network->get_layers_number() <<std::endl;
         //TrainingStrategy training_strategy(&neural_network ,&data_set);

         // ask david
         TrainingStrategy training_strategy(&(*(s-> getModelPtr(TrainNNptr->mid))) ,&data_set);

         
         
         // set Optimization Method  -------------------------------------------------------------
         if(TrainNNptr->optimization_method == GRADIENT__DESCENT){
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::GRADIENT_DESCENT);
         }
         else if(TrainNNptr->optimization_method == CONJUGATE__GRADIENT)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::CONJUGATE_GRADIENT);
         }
         else if(TrainNNptr->optimization_method == QUASI__NEWTON_METHOD)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD);
         }
         else if(TrainNNptr->optimization_method == LEVENBERG__MARQUARDT_ALGORITHM)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM);
         }
         else if(TrainNNptr->optimization_method == STOCHASTIC__GRADIENT_DESCENT)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
         }
         else if(TrainNNptr->optimization_method == ADAPTIVE__MOMENT_ESTIMATION)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION);
             
         }
         else{
             cout << "optimization_method not choosen " <<std::endl;
         }
         // end set optimization method ---------------------------------------------------------------
         


         // set Loss Method ------------------------------------------------------------------------
         if(TrainNNptr->lose_method == Sum_Squared_Error){
             training_strategy.set_loss_method(TrainingStrategy::LossMethod::SUM_SQUARED_ERROR);
         }
         else if(TrainNNptr->lose_method == Mean_Squared_Error)
         {
             training_strategy.set_loss_method(TrainingStrategy::LossMethod::MEAN_SQUARED_ERROR);
         }
         else if(TrainNNptr->lose_method == Normalized_Squared_Error)
         {
             training_strategy.set_loss_method(TrainingStrategy::LossMethod::NORMALIZED_SQUARED_ERROR);
         }
         else if(TrainNNptr->lose_method == Minkowski_Error)
         {
             training_strategy.set_loss_method(TrainingStrategy::LossMethod::MINKOWSKI_ERROR);
         }
         else if(TrainNNptr->lose_method == Weighted_Squared_Error)
         {
             training_strategy.set_loss_method(TrainingStrategy::LossMethod::WEIGHTED_SQUARED_ERROR);
         }
         else if(TrainNNptr->lose_method == Cross_Entropy_Error)
         {
             training_strategy.set_loss_method(TrainingStrategy::LossMethod::CROSS_ENTROPY_ERROR );
             
         }
         else{
             cout << "lose_method not choosen " <<std::endl;
         }

         // end set Loss Method ------------------------------------------------------------------------
         
         std::cout << "zzzzzzzzzzzzzzzzzzzzzzzzzzz" << std::endl; 
         std::cout << neural_network->get_layers_number() << std::endl; 
         std::cout << (neural_network->get_layer_pointer(0))->get_neurons_number() << std::endl;
         std::cout << (neural_network->get_layer_pointer(1))->get_neurons_number() << std::endl;
         std::cout << (neural_network->get_layer_pointer(2))->get_neurons_number() << std::endl;
         std::cout << (neural_network->get_layer_pointer(3))->get_neurons_number() << std::endl;
         std::cout << (neural_network->get_trainable_layers_pointers()(0))->get_type_string() << std::endl;
         std::cout << (neural_network->get_trainable_layers_pointers()(1))->get_type_string() << std::endl;
         // do NN trainig
         training_strategy.perform_training(); 
         
         //get weitghts test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         Tensor< float, 1 > parameters1 = neural_network->get_parameters();
         std::cout << parameters1 << std::endl;
         std::cout << "bbbb" << std::endl; 
         //end 
         loss_val = 0;

         // ask david
         //TrainingResults results;
         // Tensor<float , 1> rrr = results.get_training_error();

         ERL_NIF_TERM loss_val_term = enif_make_int(env, loss_val);
         if(enif_send(NULL,&(TrainNNptr->pid), env,loss_val_term)){
             printf("enif_send succeed\n");
         }
         else printf("enif_send failed\n");

         delete TrainNNptr;
         return 0;
         //return enif_make_string(env, "end TRAIN mode", ERL_NIF_LATIN1);

}