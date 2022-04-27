#pragma once 
/**
 * @file create.h
 * 
 * Create a neural network 
 * Given model parameters creates layers and generates an opennn neural network model 
 * 
 * @author Evgeni
 * @brief 
 * @version 0.1
 * @date 2021-12-02
 * 
 * @copyright Copyright (c) 2021
 * 
 */
//#include <iostream>
#include <vector>
#include "support.h"
#include "ModelParams.h"
#include "nifppEigenExtensions.h"
#include "choose_activation_function.h"
#include "CustumNN.h"
#include <map>
#include "../opennn/opennn/opennn.h"

//All enum defintions are defined in defnitions.h
#include "definitionsNN.h"


using namespace OpenNN;

/*
struct CreateNN {

    unsigned long modelId;
    int modelType;
    int scaling_method;
    nifpp::Tensor1D<Index> layer_types;
    nifpp::Tensor1D<Index> neural_network_architecture;
    nifpp::Tensor1D<Index> activations_functions;
};
*/

static ERL_NIF_TERM create_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    
    ModelParams modelParamsInst;
    unsigned long modelId;
    int modelType;
    int scaling_method;
    //int optimization_method;
    std::shared_ptr<nifpp::Tensor1D<Index>> layer_types;
    std::shared_ptr<nifpp::Tensor1D<Index>> neural_network_architecture;
    std::shared_ptr<nifpp::Tensor1D<Index>> activations_functions;
    
   

    
         // get data from erlang ----------------------------------------------------------------------------------------
        try{  
         nifpp::get_throws(env,argv[0],modelId);
         nifpp::get_throws(env,argv[1],modelType);
         nifpp::get_throws(env,argv[2],scaling_method); 
         nifpp::getTensor1D(env,argv[3],layer_types); 
         nifpp::getTensor1D(env,argv[4],neural_network_architecture);
         nifpp::getTensor1D(env,argv[5],activations_functions);
         
                  
        }   
     
        catch(...){
           return enif_make_string(env, "catch - get data from erlang", ERL_NIF_LATIN1);
            //return enif_make_badarg(env);
        }              
        //--------------------------------------------------------------------------------------------------------------
         

         
         // creat neural network . typy + layers number and size. -------------------------------------------------------------
        
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = std::make_shared<OpenNN::NeuralNetwork>();
        
        try{
       
         if (modelType == E_APPROXIMATION){     
             neural_network->set(NeuralNetwork::Approximation,*neural_network_architecture);     
                   
         }                                                           
         else if(modelType == E_CLASSIFICATION){     
             neural_network->set(NeuralNetwork::Classification,*neural_network_architecture); 
             
         }                                                           
         else if(modelType == E_FORECASTING){   
             neural_network->set(NeuralNetwork::Forecasting,*neural_network_architecture);      
               
         }

         else if(modelType == E_NCODER_DECODER){   
             //neural_network->set(NeuralNetwork::Forecasting,neural_network_architecture);      
               
         }

         else if(modelType == E_CUSTOMNN)
         { 
             shared_ptr<CustumNN> customNNPtr = std::make_shared<CustumNN>();
             neural_network = customNNPtr;
             
             customNNPtr->setCustumNN(neural_network_architecture, layer_types, activations_functions); 
             std::cout << "start CustumNN" << std::endl; 
             //CustumNN custumNN;
             

             
         } //CUSTOMNN
        } //try

        catch(...){
           return enif_make_string(env, "catch - select model type", ERL_NIF_LATIN1);
        } 

         

         
        
        try{ 
         // set scaling method for scaling layer ---------------------------------------------------------------------------
         //std::cout<< neural_network->get_layer_pointer(0)->get_type_string() <<std::endl;
         //std::cout<< neural_network->get_layer_pointer(0)->get_neurons_number()<<std::endl;
         ScalingLayer* scaling_layer_pointer = neural_network->get_scaling_layer_pointer();

         if(scaling_method == E_ScalingMethods_NoScaling)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::NoScaling);
        }
        else if(scaling_method == E_ScalingMethods_MinimumMaximum)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::MinimumMaximum);
        }
        else if(scaling_method == E_ScalingMethods_MeanStandardDeviation)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::MeanStandardDeviation);
        }
        else if(scaling_method == E_ScalingMethods_StandardDeviation)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::StandardDeviation);
        }
        //else if(scaling_method == ScalingMethods_Logarithm)  
        //{
        //    scaling_layer_pointer->set_scaling_methods(ScalingLayer::Logarithm);   //Logarithm exists in opennn site but commpiler dont recognaize it. 
        //}
        //------------------------------------------------------------------------------------------------------------------
        } //try

        catch(...){
           return enif_make_string(env, "catch - choose scaling method", ERL_NIF_LATIN1);
        } 
         



         
        // set activation functions for trainable layers -------------------------------------------------------------------
        try{ 
             chooseActivationFunction(neural_network , activations_functions); //TODO move to custom NN
        }

        catch(...){
           return enif_make_string(env, "catch - choose activation functions", ERL_NIF_LATIN1);
        } 
         //---------------------------------------------------------------------------------------------------------------
         

         
         
        // singelton part ----------------------------------------------------------------------------------------------
        try{ 
         std::shared_ptr<OpenNN::NeuralNetwork> modelPtr(neural_network);
 
         // Create the singleton instance
         opennnBridgeController *s = s->GetInstance();

         // Put the model record to the map with modelId
         std::cout<< "your model ID is: " <<std::endl;
         std::cout<< modelId <<std::endl;
         s->setData(modelPtr, modelId);  
        }

        catch(...){
           return enif_make_string(env, "catch - singelton part", ERL_NIF_LATIN1);
        } 
          
         return enif_make_string(env, "end create mode", ERL_NIF_LATIN1);
         //-------------------------------------------------------------------------------------------------------------   
                                                              
}  // end creat mode 







//-------------------------------------------------------------------------------------------------------------
         /*
         // get weights test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         Tensor< float, 1 > parameters = neural_network->get_parameters();
         std::cout << parameters << std::endl;
         std::cout << "bbbb" << std::endl; 
         // end
         */
         
         /*
         //chech the inputs from erlang and neural network architecture ---------------------------------------------------
         Index layer_num = neural_network->get_layers_number();
         std::cout<< layer_num <<std::endl;
         std::cout<< neural_network_architecture(0) <<std::endl;
         std::cout<< neural_network_architecture(1) <<std::endl;
         std::cout<< neural_network_architecture(2) <<std::endl;
         //std::cout<< neural_network_architecture(3) <<std::endl;
         int si = (neural_network->get_trainable_layers_pointers()).size() ;
         std::cout<< si <<std::endl;
         string type1 = (neural_network->get_trainable_layers_pointers()(1))->get_type_string();
         std::cout<<  type1 <<std::endl;
         //------------------------------------------------------------------------------------------------------
         */
         //return enif_make_string(env, "catch - problem in try", ERL_NIF_LATIN1);



         /*
               for(int i = 0; i < (int)((neural_network->get_trainable_layers_pointers()).size() ); i++){
          
             //Layer::Type layer_type = neural_network.get_layer_pointer(i)->get_type();
          if( (neural_network->get_trainable_layers_pointers()(i))->get_type_string() == "Perceptron" );
          {
            if(activations_functions(i) == E_AF_Threshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::Threshold);
            }

            if(activations_functions(i) == E_AF_SymmetricThreshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::SymmetricThreshold);
            }

            //if(activations_functions(i) == Logistic){ //problem with Logistic ,the compiler dont like it
             //     dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::Logistic);
            //}

            if(activations_functions(i) == E_AF_HyperbolicTangent){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::HyperbolicTangent);
            }
          }

          if((neural_network->get_trainable_layers_pointers()(i))->get_type_string() == "Probabilistic")
          {
            if(activations_functions(i) == E_AF_Binary){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Binary);
            }

           // if(activations_functions(i) == Logistic){  //problem with Logistic ,the compiler dont like it
                //  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Logistic);
           // }

            if(activations_functions(i) == E_AF_Competitive){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Competitive);
            }

            if(activations_functions(i) == E_AF_Softmax){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Softmax);
            }

         }
         
         }
         */