#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include "CustumNN.h"
#include <map>


#include "../opennn/opennn/opennn.h"
#include "bridgeController.h"
#include "create.h"
#include "train.h"
#include "get_set_weights.h"
#include "helpFunc.h"

#include "nifppEigenExtensions.h"

using namespace OpenNN;

#define DEBUG_CREATE_NIF 0


/*

enum ModuleType {APPROXIMATION = 1, CLASSIFICATION = 2, FORECASTING = 3 , ENCODER_DECODER = 4, CUSTUMNN = 5};

enum ScalingMethods {NoScaling = 1 , MinimumMaximum = 2 , MeanStandardDeviation = 3 , StandardDeviation = 4 , Logarithm = 5};
   
enum ActivationFunction {Threshold = 1, SymmetricThreshold = 2 , Logistic = 3 , HyperbolicTangent = 4 ,
                         Linear = 5 , RectifiedLinear = 6 , ExponentialLinear = 7 , ScaledExponentialLinear = 8 ,
                         SoftPlus = 9 , SoftSign = 10 , HardSigmoid = 11 , Binary = 12 , Competitive = 14 , Softmax = 15 };

enum LayerType {scaling = 1, convolutional = 2 , perceptron = 3 , pooling = 4 , probabilistic = 5 ,
                longShortTermMemory = 6 , recurrent = 7 , unscaling = 8 , bounding = 9 };

enum OptimizationMethod {GRADIENT__DESCENT = 1, CONJUGATE__GRADIENT = 2, QUASI__NEWTON_METHOD = 3 ,
                         LEVENBERG__MARQUARDT_ALGORITHM = 4, STOCHASTIC__GRADIENT_DESCENT = 5 , ADAPTIVE__MOMENT_ESTIMATION = 6};
               
enum LossMethod {Sum_Squared_Error = 1, Mean_Squared_Error = 2, Normalized_Squared_Error = 3 ,
                         Minkowski_Error = 4, Weighted_Squared_Error = 5 , Cross_Entropy_Error = 6};
*/               


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

/*
struct TrainNN {

    long int mid;
    int optimization_method;
    int lose_method;
    Eigen::Tensor<float,2> data;

    ErlNifTid tid;
    ErlNifPid pid;
};
*/

struct PredictNN {

    unsigned long mid;
    nifpp::Tensor1D<float> prediction_data;

    ErlNifTid tid;
    ErlNifPid pid;
};


static ERL_NIF_TERM predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         
         //PredictNN Predict;
         long int mid;
         Eigen::Tensor<float,2> data;
         ErlNifPid pid;

         enif_self(env, &pid);
         
         opennnBridgeController *s = s->GetInstance();
        
         
         nifpp::get_throws(env, argv[0], mid); // get model id
         nifpp::getTensor2D(env,argv[1],data); // get data for prediction
          
         //get neural network from singelton         
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(mid); 
         cout << neural_network->get_layers_number() <<std::endl;
         std::cout<<  "aaa" <<std::endl;
         Tensor< float, 2 > calculate_outputs =  neural_network->calculate_outputs(data);
         std::cout<<  "bbb" <<std::endl;
         /*
         std::cout<<  "bbb" <<std::endl;
         std::cout<< neural_network->calculate_outputs(data) <<std::endl;
         std::cout<<  "aaa" <<std::endl;
         */

         ERL_NIF_TERM prediction = nifpp::makeTensor2D(env, calculate_outputs);
          
         if(enif_send(NULL,&(pid), env,prediction)){
             printf("enif_send succeed\n");
         }
         else printf("enif_send failed\n");

         return enif_make_string(env, "end PREDICT mode", ERL_NIF_LATIN1);

         
     //return enif_make_int(env,0);

}  //end PREDICT mode



static ERL_NIF_TERM trainn_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
         
         //std::shared_ptr<TrainNN> TrainNNptr = std::make_shared<TrainNN>();
         TrainNN* TrainNNptr = new TrainNN();
        try{
         nifpp::get_throws(env, argv[0],TrainNNptr->mid); // model id
         nifpp::get_throws(env, argv[1],TrainNNptr->optimization_method);
         nifpp::get_throws(env, argv[2],TrainNNptr->lose_method);
         nifpp::getTensor2D(env,argv[3],TrainNNptr->data);
         ErlNifPid pid;
         enif_self(env, &pid);
         TrainNNptr->pid = pid;
        }
        catch(...){
           return enif_make_string(env, "catch - get data from erlang", ERL_NIF_LATIN1);
        }       
 
         int res = enif_thread_create((char*)"trainModule", &(TrainNNptr->tid), trainFun, TrainNNptr, 0);
             
         return enif_make_string(env, "end comunication", ERL_NIF_LATIN1);


}  //end trainn_nif






static ErlNifFunc nif_funcs[] =
{
    {"create_nif", 6 , create_nif},
    //{"train_nif", 4 , train_nif},
    {"trainn_nif", 4 , trainn_nif},
    {"predict_nif", 2 , predict_nif},
    {"printTensor",2, printTensor},
    {"get_weights_nif",1, get_weights_nif}
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
