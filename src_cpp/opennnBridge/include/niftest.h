#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include "CustumNN.h"
#include <map>


//#include "Eigen/Core"
//#include "unsupported/Eigen/CXX11/Tensor"
#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"
#include "bridgeController.h"
#include "nifppEigenExtensions.h"
//#include "train.h"

#include "nifppEigenExtensions.h"

using namespace OpenNN;

#define DEBUG_CREATE_NIF 0

//enum ModuleMode {CREATE = 0, TRAIN = 1, PREDICT = 2};

enum ModuleType {APPROXIMATION = 1, CLASSIFICATION = 2, FORECASTING = 3 , ENCODER_DECODER = 4, CUSTUMNN = 5};

enum ScalingMethods {NoScaling = 1 , MinimumMaximum = 2 , MeanStandardDeviation = 3 , StandardDeviation = 4 , Logarithm = 5};
   
enum ActivationFunction {Threshold = 1, SymmetricThreshold = 2 , Logistic = 3 , HyperbolicTangent = 4 ,
                         Linear = 5 , RectifiedLinear = 6 , ExponentialLinear = 7 , ScaledExponentialLinear = 8 ,
                         SoftPlus = 9 , SoftSign = 10 , HardSigmoid = 11 , Binary = 12 , Competitive = 14 , Softmax = 15 };

//enum LayerType {scaling = 1, convolutional = 2 , perceptron = 3 , pooling = 4 , probabilistic = 5 ,
                //longShortTermMemory = 6 , recurrent = 7 , unscaling = 8 , bounding = 9 };

enum OptimizationMethod {GRADIENT__DESCENT = 1, CONJUGATE__GRADIENT = 2, QUASI__NEWTON_METHOD = 3 ,
                         LEVENBERG__MARQUARDT_ALGORITHM = 4, STOCHASTIC__GRADIENT_DESCENT = 5 , ADAPTIVE__MOMENT_ESTIMATION = 6};
               
enum LossMethod {Sum_Squared_Error = 1, Mean_Squared_Error = 2, Normalized_Squared_Error = 3 ,
                         Minkowski_Error = 4, Weighted_Squared_Error = 5 , Cross_Entropy_Error = 6};
               




//TODO improve
inline std::string  Tensor2str(nifpp::Tensor3D<float> &inputTensor)
{
    std::string outputStr = "";
    auto dims = inputTensor.dimensions();
    for(int x=0; x < (int)dims[0] ; x++)
    {
        for(int y=0; y < (int)dims[1]; y++)
        {
            for(int z=0; z < (int)dims[2]; z++)
            {
                outputStr += to_string(static_cast<float>(inputTensor(x,y,z)));
                if(z < ((int) dims[2] - 1))
                {
                    outputStr += ",";
                }
            }
            outputStr += "\n";
        }
        outputStr += "\n";
    }
    return outputStr;
}

static ERL_NIF_TERM printTensor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::cout<<"printTensor"<<std::endl;
    nifpp::str_atom atomType;
    nifpp::get_throws(env,argv[1],atomType);
    if(atomType == "float")
    {
        nifpp::Tensor3D<float> newTensor; 
        nifpp::getTensor3D(env,argv[0],newTensor);
    }
    else if(atomType == "integer")
    {
        nifpp::Tensor3D<int> newTensor; 
        nifpp::getTensor3D(env,argv[0],newTensor);
    }
    //std::cout<<"Received Tensor: "<<std::endl;
   // std::cout<<Tensor2str(newTensor)<<std::endl;
   return enif_make_string(env, "Hello world! @@@@@", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM jello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
        std::cout << "jello";
        return enif_make_string(env, "Hello world @@@!", ERL_NIF_LATIN1);

}



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
         nifpp::get_throws(env,argv[0],modelParamsInst._modelId);
         nifpp::get_throws(env,argv[1],modelParamsInst._modelType);
         nifpp::get_throws(env,argv[2],scaling_method); 
         nifpp::getTensor1D(env,argv[3],layer_types); 
         nifpp::getTensor1D(env,argv[4],neural_network_architecture);
         nifpp::getTensor1D(env,argv[5],activations_functions);

         nifpp::get_throws(env,argv[0],modelId);
         nifpp::get_throws(env,argv[1],modelType);



        
        //--------------------------------------------------------------------------------------------------------------
         
         // set modelParamsInst ----------------------------------------------------------------------------------------
         

        
         //----------------------------


         //Tensor<Index, 1> neural_network_architecture(3);
         //neural_network_architecture.setValues({1, 3, 1});

         // creat neural network . typy + layers number and size. -------------------------------------------------------------
         //NeuralNetwork *neural_network = new NeuralNetwork(); // it's not good 
         
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = std::make_shared<OpenNN::NeuralNetwork>();


         if (modelParamsInst._modelType == APPROXIMATION){     
             neural_network->set(NeuralNetwork::Approximation,neural_network_architecture);     
                   
         }                                                           
         else if(modelParamsInst._modelType == CLASSIFICATION){     
             neural_network->set(NeuralNetwork::Classification,neural_network_architecture); 
                         
         }                                                           
         else if(modelParamsInst._modelType == FORECASTING){   
             neural_network->set(NeuralNetwork::Forecasting,neural_network_architecture);      
               
         }

         else if(modelParamsInst._modelType == ENCODER_DECODER){   
             //neural_network->set(NeuralNetwork::Forecasting,neural_network_architecture);      
               
         }

         else if(modelParamsInst._modelType == CUSTUMNN){ 
             //OpenNN::Layer* L[] = 
             std::cout << "start CustumNN" << std::endl; 
             //CustumNN custumNN;
             
             //  creat neural networl layers
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
             
             return enif_make_string(env, "catch - problem in try5", ERL_NIF_LATIN1); 
         }
         //-------------------------------------------------------------------------------------------------------------


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
         s->setData(modelPtr, modelParamsInst._modelId);  
          //return enif_make_string(env, "catch - problem in try1", ERL_NIF_LATIN1);
          return enif_make_string(env, "try2", ERL_NIF_LATIN1);
         //-------------------------------------------------------------------------------------------------------------   
    }   
     
    
     catch(...){
           return enif_make_string(env, "catch - problem in try2", ERL_NIF_LATIN1);
            //return enif_make_badarg(env);
     }                                                             
            
    
}  // end creat mode 

static ERL_NIF_TERM train_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
         

         TrainParams* trainptr =  new TrainParams();
         int optimization_method;

         Eigen::Tensor<float,2> data;

         nifpp::get_throws(env, argv[0],trainptr->_mid); // model id
         nifpp::get_throws(env, argv[1],optimization_method);
         nifpp::getTensor2D(env,argv[2],data);
         
         DataSet data_set;
         data_set.set_data(data);
         
         // Get the singleton instance
         opennnBridgeController *s = s->GetInstance();
         

         // Get the model from the singleton
         NeuralNetwork neural_network = *(s-> getModelPtr(trainptr->_mid));
         
         cout << neural_network.get_layers_number() <<std::endl;
         TrainingStrategy training_strategy(&neural_network ,&data_set);

         // ask david
         //TrainingStrategy training_strategy(&(*(s-> getModelPtr(trainptr->_mid))) ,&data_set);
         
         // set Optimization Method  -------------------------------------------------------------
         if(optimization_method == GRADIENT__DESCENT){
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::GRADIENT_DESCENT);
         }
         else if(optimization_method == CONJUGATE__GRADIENT)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::CONJUGATE_GRADIENT);
         }
         else if(optimization_method == QUASI__NEWTON_METHOD)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD);
         }
         else if(optimization_method == LEVENBERG__MARQUARDT_ALGORITHM)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM);
         }
         else if(optimization_method == STOCHASTIC__GRADIENT_DESCENT)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
         }
         else if(optimization_method == ADAPTIVE__MOMENT_ESTIMATION)
         {
             training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION);
             
         }
         else{
             cout << "optimization_method not choosen " <<std::endl;
         }
         // end set optimization method ---------------------------------------------------------------
         


         // set Loss Method ------------------------------------------------------------------------
         

         // end set Loss Method ------------------------------------------------------------------------
         

         // do NN trainig
         training_strategy.perform_training();  

         return enif_make_string(env, "catch - problem in try2", ERL_NIF_LATIN1);
         
}  //end TRAIN mode  
    
    
static ERL_NIF_TERM predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         
         
         long int mid;
         opennnBridgeController *s = s->GetInstance();
         Eigen::Tensor<float,2> data;
         nifpp::get_throws(env, argv[0], mid); // get model id
         nifpp::getTensor2D(env,argv[1],data);            // get data for prediction
         NeuralNetwork neural_network = *(s-> getModelPtr(mid)); //get neural network from singelton
         cout << neural_network.get_layers_number() <<std::endl;
         
         //Tensor< float, 2 > calculate_outputs =  neural_network.calculate_outputs(data);
         std::cout<<  "bbb" <<std::endl;
         std::cout<< neural_network.calculate_outputs(data) <<std::endl;
         std::cout<<  "aaa" <<std::endl;
         return enif_make_string(env, "catch - problem in try3", ERL_NIF_LATIN1);

         
     
     //}

     return enif_make_int(env,0);
    //return enif_make_string(env, "Hello world! @@@@@", ERL_NIF_LATIN1);
/*
#if DEBUG_CREATE_NIF
            std::cout << "Optimizers::opt_t optimizer: " << optimizer << '\n';
            std::cout << "act_types_vec: " << act_types_vec << '\n';
            std::cout << "modelParamPtr.layers_sizes: " << modelParamPtr.layers_sizes << '\n';
            std::cout << "modelParamPtr.learning_rate: " << modelParamPtr.learning_rate << '\n';
            std::cout << "Start creating the module." << '\n';
#endif
*/
}  //end PREDICT mode

static ErlNifFunc nif_funcs[] =
{
    {"create_nif", 6 , create_nif},
    {"train_nif", 3 , train_nif},
    {"predict_nif", 2 , predict_nif},
    {"printTensor",2, printTensor}
   // {"jello", 1, jello}
};

// TODO: Think about using this feature in the future
// load_info is the second argument to erlang:load_nif/2.
// *priv_data can be set to point to some private data if the library needs to keep a state between NIF calls.
// enif_priv_data returns this pointer. *priv_data is initialized to NULL when load is called.
// The library fails to load if load returns anything other than 0. load can be NULL if initialization is not needed.
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    //nifpp::register_resource<GetcppBridgeController>(env, nullptr, "GetcppBridgeController");
    //nifpp::register_resource<cppBridgeController>(env, nullptr, "cppBridgeController");
   // nifpp::register_resource<SANN::Model>(env, nullptr, "cppBridgeController");
    return 0;
}

// This is the magic macro to initialize a NIF library. It is to be evaluated in global file scope.
// ERL_NIF_INIT(MODULE, ErlNifFunc funcs[], load, NULL, upgrade, unload)
// MODULE -  The first argument must be the name of the Erlang module as a C-identifier. It will be stringified by the macro.
// ErlNifFunc - The second argument is the array of ErlNifFunc structures containing name, arity, and function pointer of each NIF.
// load -  is called when the NIF library is loaded and no previously loaded library exists for this module.
// NULL - The fourth argument NULL is ignored. It was earlier used for the deprecated reload callback which is no longer supported since OTP 20.
// The remaining arguments are pointers to callback functions that can be used to initialize the library.
// They are not used in this simple example, hence they are all set to NULL.
ERL_NIF_INIT(niftest, nif_funcs, load, NULL, NULL, NULL)

//ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)
