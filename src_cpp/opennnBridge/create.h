#pragma once 
/**
 * @file create.h
 * 
 * Create a neural network 
 * Given model parameters creates layers and generates an opennn neural network model 
 * 
 * @authors  Evgeni Andrachnic & David Leon & Ziv Moldavsky
 * @brief 
 * @version 0.1
 * @date 2021-12-02
 * 
 * @copyright Copyright (c) 2021 Nerlnet
 * 
 */

#include "ModelParams.h"
#include "openNNExtensionFunction.h"
#include "CustumNN.h"
#include "../simple-cpp-logger/include/Logger.h"
#include "bridgeController.h"

using namespace opennn;

static ERL_NIF_TERM create_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) // David
{
    ModelParams modelParamsInst;
    unsigned long modelId;
    int modelType;
    int scaling_method;
    //int optimization_method;
    iTensor1DPtr layer_types;
    iTensor1DPtr neural_network_architecture;
    iTensor1DPtr activations_functions;

    
         // get data from erlang ----------------------------------------------------------------------------------------
        try{  
         nifpp::get_throws(env,argv[0],modelId);
         nifpp::get_throws(env,argv[1],modelType);
         nifpp::get_throws(env,argv[2],scaling_method); 
         nifpp::get_tensor_1d_from_erl_list(env,argv[3],layer_types); 
         nifpp::get_tensor_1d_from_erl_list(env,argv[4],neural_network_architecture);
         nifpp::get_tensor_1d_from_erl_list(env,argv[5],activations_functions);
           
        }   
     
        catch(...){
           cout << "catch - get data from erlang"  << endl;
           return enif_make_string(env, "catch - get data from erlang", ERL_NIF_LATIN1);
     
        } 
                    
        //--------------------------------------------------------------------------------------------------------------
         
         // creat neural network . typy + layers number and size. -------------------------------------------------------------
        
         std::shared_ptr<opennn::NeuralNetwork> neural_network = std::make_shared<opennn::NeuralNetwork>();
         
        try{
             
        if(modelType == E_FEDERATED_CLIENT || modelType == E_FEDERATED_SERVER)
        {
            modelType = E_CUSTOMNN;
        }

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
       
        } //try

        catch(...){
           return enif_make_string(env, "catch - select model type", ERL_NIF_LATIN1);
        } 

         cout << "Done setting CustomNN" << std::endl;
        try{ 
         // set scaling method for scaling layer ---------------------------------------------------------------------------
         ScalingLayer* scaling_layer_pointer = neural_network->get_scaling_layer_pointer();
         cout << "scaling method = " << scaling_method << std::endl;
         if(scaling_method == E_ScalingMethods_NoScaling)
        {
            scaling_layer_pointer->set_scalers(opennn::Scaler::NoScaling);
        }
        else if(scaling_method == E_ScalingMethods_MinimumMaximum)
        {
            scaling_layer_pointer->set_scalers(opennn::Scaler::MinimumMaximum);
        }
        else if(scaling_method == E_ScalingMethods_MeanStandardDeviation)
        {
            scaling_layer_pointer->set_scalers(opennn::Scaler::MeanStandardDeviation);
        }
        else if(scaling_method == E_ScalingMethods_StandardDeviation)
        {
            scaling_layer_pointer->set_scalers(opennn::Scaler::StandardDeviation);
        }
        else if(scaling_method == E_ScalingMethods_Logarithm)  
        {
           // scaling_layer_pointer->set_scaling_methods(opennn::Scaler::::Logarithm);   //Logarithm exists in opennn site but commpiler dont recognaize it. 
        }
        //------------------------------------------------------------------------------------------------------------------
          cout << "Done setting NN scalers" << std::endl;
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
            std::shared_ptr<opennn::NeuralNetwork> modelPtr(neural_network);
            // Create the singleton instance
            opennnBridgeController& onnBrCtrl = opennnBridgeController::GetInstance();

            // Put the model record to the map with modelId
            onnBrCtrl.setData(modelPtr, modelId , modelType);
            LogInfo<< "New model is assigned - ID " << modelId << std::endl;
        }
        

        catch(...){
            LogError << "[Bridge Controller] Issue with model creation and assigment" << std::endl;
            return enif_make_string(env, "catch - singelton part", ERL_NIF_LATIN1);
        } 
 
         return enif_make_string(env, "end create mode", ERL_NIF_LATIN1);
         //-------------------------------------------------------------------------------------------------------------   
                                                              
}  // end creat mode 
