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
#include "openNNExtensionFunction.h"
#include "CustumNN.h"
#include "Autoencoder.h"
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
           cout << "catch - get data from erlang"  << endl;
           return enif_make_string(env, "catch - get data from erlang", ERL_NIF_LATIN1);
     
        } 
                    
        //--------------------------------------------------------------------------------------------------------------
         

         
         // creat neural network . typy + layers number and size. -------------------------------------------------------------
        
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = std::make_shared<OpenNN::NeuralNetwork>();
         
        try{
             
         if (modelType == E_APPROXIMATION){     
             neural_network->set(NeuralNetwork::ProjectType::Approximation,*neural_network_architecture);     
                   
         }                                                           
         else if(modelType == E_CLASSIFICATION){     
             neural_network->set(NeuralNetwork::ProjectType::Classification,*neural_network_architecture); 
          
         }                                                           
         else if(modelType == E_FORECASTING){   
             neural_network->set(NeuralNetwork::ProjectType::Forecasting,*neural_network_architecture);      
               
         }

         else if(modelType == E_NCODER_DECODER){   
             //neural_network->set(NeuralNetwork::ProjectType::Forecasting,neural_network_architecture);      
               
         }

         else if(modelType == E_CUSTOMNN )
         { 
             
             shared_ptr<CustumNN> customNNPtr = std::make_shared<CustumNN>(); 
             neural_network = customNNPtr;
             customNNPtr->setCustumNN(neural_network_architecture, layer_types, activations_functions); 
             
         } //CUSTOMNN

         else if(modelType ==  E_AE)
         { 

             shared_ptr<Autoencoder> autoencoderPtr = std::make_shared<Autoencoder>();
             neural_network = autoencoderPtr;
             autoencoderPtr->setCustumNN(neural_network_architecture, layer_types, activations_functions); 
             
         } //Autoencoder

          else if(modelType == E_AEC)
         { 
             double k = 0.1;
             shared_ptr<AutoencoderClassifier> autoencoderClassifierPtr = std::make_shared<AutoencoderClassifier>(k);
             neural_network = autoencoderClassifierPtr;
             autoencoderClassifierPtr->setCustumNN(neural_network_architecture, layer_types, activations_functions); 
             
         } //AutoencoderClassifier
       
       
        } //try

        catch(...){
           return enif_make_string(env, "catch - select model type", ERL_NIF_LATIN1);
        } 

         
        try{ 
         // set scaling method for scaling layer ---------------------------------------------------------------------------
         ScalingLayer* scaling_layer_pointer = neural_network->get_scaling_layer_pointer();
         if(scaling_method == E_ScalingMethods_NoScaling)
        {
            scaling_layer_pointer->set_scalers(OpenNN::Scaler::NoScaling);
        }
        else if(scaling_method == E_ScalingMethods_MinimumMaximum)
        {
            scaling_layer_pointer->set_scalers(OpenNN::Scaler::MinimumMaximum);
        }
        else if(scaling_method == E_ScalingMethods_MeanStandardDeviation)
        {
            scaling_layer_pointer->set_scalers(OpenNN::Scaler::MeanStandardDeviation);
        }
        else if(scaling_method == E_ScalingMethods_StandardDeviation)
        {
            scaling_layer_pointer->set_scalers(OpenNN::Scaler::StandardDeviation);
        }
        else if(scaling_method == E_ScalingMethods_Logarithm)  
        {
           // scaling_layer_pointer->set_scaling_methods(OpenNN::Scaler::::Logarithm);   //Logarithm exists in opennn site but commpiler dont recognaize it. 
        }
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
         s->setData(modelPtr, modelId , modelType);  
        }

        catch(...){
           return enif_make_string(env, "catch - singelton part", ERL_NIF_LATIN1);
        } 
 
         return enif_make_string(env, "end create mode", ERL_NIF_LATIN1);
         //-------------------------------------------------------------------------------------------------------------   
                                                              
}  // end creat mode 







//-------------------------------------------------------------------------------------------------------------
    
         /*
         //std::cout<< neural_network_architecture(3) <<std::endl;
         int si = (neural_network->get_trainable_layers_pointers()).size() ;
-----------------------------------------------------------------
         */
  