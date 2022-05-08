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
         if( (neural_network->get_trainable_layers_pointers()(i))->get_type() == Layer::Type::Pooling ) //"Pooling"
         {
            if(activations_functions(i) == E_Pooling_Method_NoPooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::PoolingMethod::NoPooling);
            }

            if(activations_functions(i) == E_Pooling_Method_MaxPooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::PoolingMethod::MaxPooling);
            }
            
            if(activations_functions(i) == E_Pooling_Method_AveragePooling){
                  dynamic_cast<PoolingLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_pooling_method(PoolingLayer::PoolingMethod::AveragePooling);
            }
         }

        
         //Perceptron + LongShortTermMemory + Recurrent + Convolutional
         if( ((neural_network->get_trainable_layers_pointers()(i))->get_type() == Layer::Type::Perceptron) 
             || ((neural_network->get_trainable_layers_pointers()(i))->get_type() == Layer::Type::LongShortTermMemory) 
             || ((neural_network->get_trainable_layers_pointers()(i))->get_type() == Layer::Type::Recurrent ) 
             || ( (neural_network->get_trainable_layers_pointers()(i))->get_type() == Layer::Type::Convolutional )) 
         {
            if(activations_functions(i) == E_AF_Threshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::Threshold);
            }

            if(activations_functions(i) == E_AF_SymmetricThreshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::SymmetricThreshold);
            }

            if(activations_functions(i) == E_AF_Logistic){ 
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::Logistic);
            }

            if(activations_functions(i) == E_AF_HyperbolicTangent){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::HyperbolicTangent);
            }

            if(activations_functions(i) == E_AF_Linear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::Linear);
            }

            if(activations_functions(i) == E_AF_RectifiedLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::RectifiedLinear);
            }

            if(activations_functions(i) == E_AF_ExponentialLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::ExponentialLinear);
            }

            if(activations_functions(i) == E_AF_ScaledExponentialLinear){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::ScaledExponentialLinear);
            }

            if(activations_functions(i) == E_AF_SoftPlus){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::SoftPlus);
            }
 
            if(activations_functions(i) == E_AF_SoftSign){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::SoftSign);
            }

            if(activations_functions(i) == E_AF_HardSigmoid){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::ActivationFunction::HardSigmoid);
            }
         }


         //Probabilistic
         if((neural_network->get_trainable_layers_pointers()(i))->get_type() == Layer::Type::Probabilistic) //"Probabilistic"
         {
            if(activations_functions(i) == PAF_Binary){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::ActivationFunction::Binary);
            }

            if(activations_functions(i) == PAF_Logistic){  //problem with Logistic ,the compiler dont like it
                 dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::ActivationFunction::Logistic);
            }

            if(activations_functions(i) == PAF_Competitive){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::ActivationFunction::Competitive);
            }

            if(activations_functions(i) == PAF_Softmax){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::ActivationFunction::Softmax);
            }

         }
    }
}



//Layer::Type layer_type = neural_network.get_layer_pointer(i)->get_type();
