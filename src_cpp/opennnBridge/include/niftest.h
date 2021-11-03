#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
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

enum ModuleMode {CREATE = 0, TRAIN = 1, PREDICT = 2};


 
   
enum ActivationFunction {Threshold = 1, SymmetricThreshold = 2 , Logistic = 3 , HyperbolicTangent = 4 ,
                         Linear = 5 , RectifiedLinear = 6 , ExponentialLinear = 7 , ScaledExponentialLinear = 8 ,
                         SoftPlus = 9 , SoftSign = 10 , HardSigmoid = 11 };

enum LayerType {scaling = 1, convolutional = 2 , perceptron = 3 , pooling = 4 , probabilistic = 5 ,
                longShortTermMemory = 6 , recurrent = 7 , unscaling = 8 , bounding = 9 };

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
        nifpp::getTensor(env,argv[0],newTensor);
    }
    else if(atomType == "integer")
    {
        nifpp::Tensor3D<int> newTensor; 
        nifpp::getTensor(env,argv[0],newTensor);
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

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    
    Tensor<Index, 1> neural_network_architecture(3);
    neural_network_architecture.setValues({1, 3, 1});
    ModelParams modelParamsInst;
    int layers_num;
    int mode;
    std::vector<int> v;
    nifpp::Tensor3D<Index> t;
    nifpp::get_throws(env,argv[0],mode);
    if(mode == CREATE){

    try{
         
         nifpp::get_throws(env,argv[1],modelParamsInst._modelId);
         nifpp::get_throws(env,argv[2],modelParamsInst._modelType); 
         //nifpp::get_throws(env,argv[3],v); 
         nifpp::getTensor(env,argv[3],t);
        //nifpp::get_throws(env,argv[3],*(modelParamsInst.GetLayersSizes()));
        // nifpp::get_throws(env,argv[4],*(modelParamsInst.GetLayersTypes()));
        // nifpp::get_throws(env,argv[5],*(modelParamsInst.GetAcvtivationList())); // list of activation functions
         
        

         Eigen::Tensor<Index,1> t1(t.size());
         for (int i =0 ; i< t.size(); i++){
         t1(i) = t(i,0,0);
         }
         
         
         NeuralNetwork *neural_network = new NeuralNetwork(NeuralNetwork::Approximation,t1); // its can be a problem

         
         if (modelParamsInst._modelType == 1){     
             neural_network->set(NeuralNetwork::Approximation,t1);     
             
               
         }                                                           
         else if(modelParamsInst._modelType == 2){     
             neural_network->set(NeuralNetwork::Classification,t1); 
                         
         }                                                           
         else if(modelParamsInst._modelType == 3){   
             neural_network->set(NeuralNetwork::Forecasting,t1);      
               
         }
         return enif_make_string(env, "catch - problem in try", ERL_NIF_LATIN1);
         

         for(int i = 0; i < (int)(neural_network->get_layers_number()); i++){
          
             //Layer::Type layer_type = neural_network.get_layer_pointer(i)->get_type();
            if(modelParamsInst.GetLayersTypes()->at(i) == scaling){
                 // dynamic_cast<ScalingLayer*>(neural_network.get_trainable_layers_pointers()(i))->set_activation_function(ScalingLayer::LongShortTermMemory);
            }

            if(modelParamsInst.GetLayersTypes()->at(i) == convolutional){
                  dynamic_cast<ConvolutionalLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(ConvolutionalLayer::Logistic);
            }

            if(modelParamsInst.GetLayersTypes()->at(i) == perceptron){
                  dynamic_cast<PerceptronLayer*>(neural_network->get_trainable_layers_pointers()(i))->set_activation_function(PerceptronLayer::HyperbolicTangent);
            }
         //dynamic_cast<ProbabilisticLayer*>(neural_network.get_trainable_layers_pointers()(1))->set_activation_function(ProbabilisticLayer::Logistic);
         }

         layers_num = (int)neural_network->get_layers_number();
         //ScalingLayer* scaling_layer_pointer = neural_network.get_scaling_layer_pointer();
         //scaling_layer_pointer->set_scaling_methods(ScalingLayer::NoScaling);

         
         //for(int i = 0; i < (int)(modelParamsInst.GetAcvtivationList().size()); i++){
             //act_types_vec.push_back(static_cast<act_t>(modelParamPtr.activation_list[i]));
         //}
    

         //set_activations_function(ProbabilisticLayer::S);
         

         std::shared_ptr<OpenNN::NeuralNetwork> modelPtr(neural_network);

         // Create the singleton instance
         opennnBridgeController *s = s->GetInstance();

         // Put the model record to the map with modelId
         s->setData(modelPtr, modelParamsInst._modelId);      
    }   
    //catch(nifpp::badarg){
     //       std::cout << "bad arg123";
     //       return enif_make_badarg(env);
    // }
     catch(...){
           return enif_make_string(env, "catch - problem in try", ERL_NIF_LATIN1);
            //return enif_make_badarg(env);
     }                                                             
            
    } //end CREATE mode
   
    else if(mode == TRAIN){


    }

    return enif_make_int(env,layers_num);
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
}

static ErlNifFunc nif_funcs[] =
{
    {"hello", 4 , hello},
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
