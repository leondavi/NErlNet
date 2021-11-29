#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include "nifppEigenExtensions.h"
//#include "CustumNN.h"
#include <map>
#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"

using namespace OpenNN;

enum ModuleType {APPROXIMATION = 1, CLASSIFICATION = 2, FORECASTING = 3 , ENCODER_DECODER = 4, CUSTUMNN = 5};

enum ScalingMethods {NoScaling = 1 , MinimumMaximum = 2 , MeanStandardDeviation = 3 , StandardDeviation = 4 , Logarithm = 5};
   
enum ActivationFunction {Threshold = 1, SymmetricThreshold = 2 , Logistic = 3 , HyperbolicTangent = 4 ,
                         Linear = 5 , RectifiedLinear = 6 , ExponentialLinear = 7 , ScaledExponentialLinear = 8 ,
                         SoftPlus = 9 , SoftSign = 10 , HardSigmoid = 11 , Binary = 12 , Competitive = 14 , Softmax = 15 };

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
    nifpp::Tensor1D<Index> layer_types;
    nifpp::Tensor1D<Index> neural_network_architecture;
    nifpp::Tensor1D<Index> activations_functions;
    
   

    try{
         // get data from erlang ----------------------------------------------------------------------------------------
       
         nifpp::get_throws(env,argv[0],modelId);
         nifpp::get_throws(env,argv[1],modelType);
         nifpp::get_throws(env,argv[2],scaling_method); 
         nifpp::getTensor1D(env,argv[3],layer_types); 
         nifpp::getTensor1D(env,argv[4],neural_network_architecture);
         nifpp::getTensor1D(env,argv[5],activations_functions);

         
        
        //--------------------------------------------------------------------------------------------------------------
         
         // set modelParamsInst ----------------------------------------------------------------------------------------
         

        
         //----------------------------


         //Tensor<Index, 1> neural_network_architecture(3);
         //neural_network_architecture.setValues({1, 3, 1});

         // creat neural network . typy + layers number and size. -------------------------------------------------------------
         //NeuralNetwork *neural_network = new NeuralNetwork(); // it's not good 
         
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = std::make_shared<OpenNN::NeuralNetwork>();


         if (modelType == APPROXIMATION){     
             neural_network->set(NeuralNetwork::Approximation,neural_network_architecture);     
                   
         }                                                           
         else if(modelType == CLASSIFICATION){     
             neural_network->set(NeuralNetwork::Classification,neural_network_architecture); 
                         
         }                                                           
         else if(modelType == FORECASTING){   
             neural_network->set(NeuralNetwork::Forecasting,neural_network_architecture);      
               
         }

         else if(modelType == ENCODER_DECODER){   
             //neural_network->set(NeuralNetwork::Forecasting,neural_network_architecture);      
               
         }

         else if(modelType == CUSTUMNN){ 
             
             std::cout << "start CustumNN" << std::endl; 
             //CustumNN custumNN;
             
             //  create neural networl layers
             for(int i = 0 ; i < neural_network_architecture.size() ; i++){
                 
                 if (layer_types[i] == scaling){
                     OpenNN::ScalingLayer* L = new ScalingLayer(); 
                     //std::shared_ptr<OpenNN::ScalingLayer> L(new OpenNN::ScalingLayer()); //not work
                     L->set(neural_network_architecture[i]);
                     neural_network->add_layer(L); 
                 }

                 if (layer_types[i] == perceptron){
                     OpenNN::PerceptronLayer* L = new PerceptronLayer();
                     //std::shared_ptr<OpenNN::PerceptronLayer> L = std::make_shared<OpenNN::PerceptronLayer>(); // not work
                     L->set_neurons_number(neural_network_architecture[i]);
                     neural_network->add_layer(L); 
                 }

                 if (layer_types[i] == probabilistic){
                     OpenNN::ProbabilisticLayer* L = new ProbabilisticLayer();
                     //std::shared_ptr<OpenNN::ProbabilisticLayer> L = std::make_shared<OpenNN::ProbabilisticLayer>(); //not work
                     L->set_neurons_number(neural_network_architecture[i]);
                     neural_network->add_layer(L); 
                 }

                 std::cout << neural_network->get_layers_number() << std::endl; 
                 std::cout << "end CustumNN" << std::endl; 
                 
             }
             
            // return enif_make_string(env, "catch - problem in try5", ERL_NIF_LATIN1); 
         }
         //-------------------------------------------------------------------------------------------------------------

         // get weights test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         Tensor< float, 1 > parameters = neural_network->get_parameters();
         std::cout << parameters << std::endl;
         std::cout << "bbbb" << std::endl; 
         // end

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

         //return enif_make_string(env, "catch - problem in try", ERL_NIF_LATIN1);
        
         
         // set scaling method for scaling layer ---------------------------------------------------------------------------
         ScalingLayer* scaling_layer_pointer = neural_network->get_scaling_layer_pointer();
         if(scaling_method == NoScaling)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::NoScaling);
        }
        else if(scaling_method == MinimumMaximum)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::MinimumMaximum);
        }
        else if(scaling_method == MeanStandardDeviation)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::MeanStandardDeviation);
        }
        else if(scaling_method == StandardDeviation)
        {
            scaling_layer_pointer->set_scaling_methods(ScalingLayer::StandardDeviation);
        }
        //else if(scaling_method == Logarithm)  
        //{
        //    scaling_layer_pointer->set_scaling_methods(ScalingLayer::Logarithm);   //Logarithm exists in opennn site but commpiler dont recognaize it. 
        //}
        //------------------------------------------------------------------------------------------------------------------



        // set activation functions for trainable layers -------------------------------------------------------------------
         for(int i = 0; i < (int)((neural_network->get_trainable_layers_pointers()).size() ); i++){
          
             //Layer::Type layer_type = neural_network.get_layer_pointer(i)->get_type();
          if( (neural_network->get_trainable_layers_pointers()(i))->get_type_string() == "Perceptron" );
          {
            if(activations_functions(i) == Threshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::Threshold);
            }

            if(activations_functions(i) == SymmetricThreshold){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::SymmetricThreshold);
            }

            //if(activations_functions(i) == Logistic){ //problem with Logistic ,the compiler dont like it
             //     dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::Logistic);
            //}

            if(activations_functions(i) == HyperbolicTangent){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::HyperbolicTangent);
            }
          }

          if((neural_network->get_trainable_layers_pointers()(i))->get_type_string() == "Probabilistic")
          {
            if(activations_functions(i) == Binary){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Binary);
            }

           // if(activations_functions(i) == Logistic){  //problem with Logistic ,the compiler dont like it
                //  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Logistic);
           // }

            if(activations_functions(i) == Competitive){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Competitive);
            }

            if(activations_functions(i) == Softmax){
                  dynamic_cast<ProbabilisticLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ProbabilisticLayer::Softmax);
            }

         }
         }
         //---------------------------------------------------------------------------------------------------------------
         
         

         
         
        // singelton part ----------------------------------------------------------------------------------------------
         std::shared_ptr<OpenNN::NeuralNetwork> modelPtr(neural_network);

         // Create the singleton instance
         opennnBridgeController *s = s->GetInstance();
         
         // Put the model record to the map with modelId
         s->setData(modelPtr, modelId);  
          //return enif_make_string(env, "catch - problem in try1", ERL_NIF_LATIN1);
          return enif_make_string(env, "end create mode", ERL_NIF_LATIN1);
         //-------------------------------------------------------------------------------------------------------------   
    }   
     
    
     catch(...){
           return enif_make_string(env, "catch - problem in try2", ERL_NIF_LATIN1);
            //return enif_make_badarg(env);
     }                                                             
               
}  // end creat mode 
