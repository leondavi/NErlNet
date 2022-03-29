#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
//#include "CustumNN.h"
#include "definitionsNN.h"
#include <map>


//#include "Eigen/Core"
//#include "unsupported/Eigen/CXX11/Tensor"
//#include <eigen3/Eigen/Core>
#include "../opennn/opennn/opennn.h"
#include "nifppEigenExtensions.h"

using namespace OpenNN;


//


void chooseActivationFunction(std::shared_ptr<OpenNN::NeuralNetwork> neural_network , nifpp::Tensor1D<Index> activations_functions)
{
   for(int i = 0; i < (int)((neural_network->get_trainable_layers_pointers()).size() ); i++){
          
         

         //Pooling
         if( (neural_network->get_trainable_layers_pointers()(i))->get_type() == 3 ) //"Pooling"
         {
            if(activations_functions(i) == E_Pooling_Method_NoPooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::NoPooling);
            }

            if(activations_functions(i) == E_Pooling_Method_MaxPooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::MaxPooling);
            }
            
            if(activations_functions(i) == E_Pooling_Method_AveragePooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::AveragePooling);
            }
         }

        
         //Perceptron + LongShortTermMemory + Recurrent + Convolutional
         if( ((neural_network->get_trainable_layers_pointers()(i))->get_type() == 2) //"Perceptron"
             || ((neural_network->get_trainable_layers_pointers()(i))->get_type() == 5) //"LongShortTermMemory"
             || ((neural_network->get_trainable_layers_pointers()(i))->get_type() == 6 ) //"Recurrent"
             || ( (neural_network->get_trainable_layers_pointers()(i))->get_type() == 1 )) //"Convolutional"
         {
            if(activations_functions(i) == E_AF_Threshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::Threshold);
            }

            if(activations_functions(i) == E_AF_SymmetricThreshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::SymmetricThreshold);
            }

            if(activations_functions(i) == E_AF_Logistic){ 
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::Logistic);
            }

            if(activations_functions(i) == E_AF_HyperbolicTangent){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::HyperbolicTangent);
            }

            if(activations_functions(i) == E_AF_Linear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::Linear);
            }

            if(activations_functions(i) == E_AF_RectifiedLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::RectifiedLinear);
            }

            if(activations_functions(i) == E_AF_ExponentialLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ExponentialLinear);
            }

            if(activations_functions(i) == E_AF_ScaledExponentialLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ScaledExponentialLinear);
            }

            if(activations_functions(i) == E_AF_SoftPlus){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::SoftPlus);
            }
 
            if(activations_functions(i) == E_AF_SoftSign){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::SoftSign);
            }

            if(activations_functions(i) == E_AF_HardSigmoid){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::HardSigmoid);
            }
         }


         //Probabilistic
         if((neural_network->get_trainable_layers_pointers()(i))->get_type() == 4) //"Probabilistic"
         {
            if(activations_functions(i) == PAF_Binary){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Binary);
            }

            if(activations_functions(i) == PAF_Logistic){  //problem with Logistic ,the compiler dont like it
                 dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Logistic);
            }

            if(activations_functions(i) == PAF_Competitive){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Competitive);
            }

            if(activations_functions(i) == PAF_Softmax){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Softmax);
            }

         }
    }
}



//Layer::Type layer_type = neural_network.get_layer_pointer(i)->get_type();
