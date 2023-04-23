/*****************************************************
 * Authors: Evgeni Andrachnic
 * 29/10/2021
 * 
 * @copyright Copyright (c) 2021 Nerlnet
 *****************************************************/ 

#pragma once 

#include <vector>
#include <string>
#include "ModelParams.h"
#include "definitionsNN.h"
#include <map>

#include "../opennn/opennn/opennn.h"
#include "nifppEigenExtensions.h"

using namespace opennn;



// openNNExtensionFunction.h
static void chooseActivationFunction(std::shared_ptr<opennn::NeuralNetwork> neural_network , iTensor1DPtr activations_functions)
{
   for(int i = 0; i < (int)((neural_network->get_trainable_layers_pointers()).size() ); i++){
          
     
         //Pooling
         if( (neural_network->get_trainable_layers_pointers()(i))->get_type() == opennn::Layer::Type::Pooling ) //"Pooling"
         {
            if((*activations_functions)(i) == E_Pooling_Method_NoPooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::PoolingMethod::NoPooling);
            }

            if((*activations_functions)(i) == E_Pooling_Method_MaxPooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::PoolingMethod::MaxPooling);
            }
            
            if((*activations_functions)(i) == E_Pooling_Method_AveragePooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::PoolingMethod::AveragePooling);
            }
         }

        
         //Perceptron + LongShortTermMemory + Recurrent + Convolutional
         if( ((neural_network->get_trainable_layers_pointers()(i))->get_type() == opennn::Layer::Type::Perceptron) //"Perceptron"
             || ((neural_network->get_trainable_layers_pointers()(i))->get_type() == opennn::Layer::Type::LongShortTermMemory) //"LongShortTermMemory"
             || ((neural_network->get_trainable_layers_pointers()(i))->get_type() == opennn::Layer::Type::Recurrent ) //"Recurrent"
             || ( (neural_network->get_trainable_layers_pointers()(i))->get_type() == opennn::Layer::Type::Convolutional )) //"Convolutional"
         {
            if((*activations_functions)(i) == E_AF_Threshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::Threshold);
            }

            if((*activations_functions)(i) == E_AF_SymmetricThreshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::SymmetricThreshold);
            }

            if((*activations_functions)(i) == E_AF_Logistic){ 
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::Logistic);
            }

            if((*activations_functions)(i) == E_AF_HyperbolicTangent){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::HyperbolicTangent);
            }

            if((*activations_functions)(i) == E_AF_Linear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::Linear);
            }

            if((*activations_functions)(i) == E_AF_RectifiedLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::RectifiedLinear);
            }

            if((*activations_functions)(i) == E_AF_ExponentialLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::ExponentialLinear);
            }

            if((*activations_functions)(i) == E_AF_ScaledExponentialLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::ScaledExponentialLinear);
            }

            if((*activations_functions)(i) == E_AF_SoftPlus){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::SoftPlus);
            }
 
            if((*activations_functions)(i) == E_AF_SoftSign){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::SoftSign);
            }

            if((*activations_functions)(i) == E_AF_HardSigmoid){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::HardSigmoid);
            }
         }
      }
      
         //Probabilistic
         int last_layar = neural_network->get_layers_pointers().size()-1;
         if(neural_network->get_layer_pointer(last_layar)->get_type() == opennn::Layer::Type::Probabilistic) //"Probabilistic"
         {
            if((*activations_functions)(last_layar) == PAF_Binary){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar))->set_activation_function(ProbabilisticLayer::ActivationFunction::Binary);
            }
            
            if((*activations_functions)(last_layar) == PAF_Logistic){  //problem with Logistic ,the compiler dont like it
                 dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar))->set_activation_function(ProbabilisticLayer::ActivationFunction::Logistic);
            }

            if((*activations_functions)(last_layar) == PAF_Competitive){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar))->set_activation_function(ProbabilisticLayer::ActivationFunction::Competitive);
            }

            if((*activations_functions)(last_layar) == PAF_Softmax){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar))->set_activation_function(ProbabilisticLayer::ActivationFunction::Softmax);    
            }

         }

         int last_layar2 = neural_network->get_layers_pointers().size()-2;
         if(neural_network->get_layer_pointer(last_layar)->get_type() == opennn::Layer::Type::Probabilistic) //"Probabilistic"
         {
       
            if((*activations_functions)(last_layar2) == PAF_Binary){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar2))->set_activation_function(ProbabilisticLayer::ActivationFunction::Binary);
            }

            if((*activations_functions)(last_layar2) == PAF_Logistic){  //problem with Logistic ,the compiler dont like it
                 dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar2))->set_activation_function(ProbabilisticLayer::ActivationFunction::Logistic);
            }

            if((*activations_functions)(last_layar2) == PAF_Competitive){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar2))->set_activation_function(ProbabilisticLayer::ActivationFunction::Competitive);
            }

            if((*activations_functions)(last_layar2) == PAF_Softmax){               
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_layer_pointer(last_layar2))->set_activation_function(ProbabilisticLayer::ActivationFunction::Softmax);         
            }

         } //end choose Probabilistic method.
    
}


