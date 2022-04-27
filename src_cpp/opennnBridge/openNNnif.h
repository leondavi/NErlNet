#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
#include "CustumNN.h"
#include <map>
#include <chrono>

using namespace std::chrono;

#include "../opennn/opennn/opennn.h"
#include "bridgeController.h"
#include "create.h"
#include "train.h"
#include "get_set_weights.h"
#include "helpFunc.h"

#include "nifppEigenExtensions.h"

using namespace OpenNN;

#define DEBUG_CREATE_NIF 0




struct PredictNN {

    long int mid;
    Eigen::Tensor<float,2> data;
    ErlNifPid pid;
    ErlNifTid tid;
};


static void* PredictFun(void* arg){ 
         //PredictNN* PredictNNptr = (PredictNN*)arg;
         PredictNN* PredictNNptr = reinterpret_cast<PredictNN*>(arg);
         ErlNifEnv *env = enif_alloc_env();    
         opennnBridgeController *s = s->GetInstance();
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(PredictNNptr->mid);
         Tensor< float, 2 > calculate_outputs =  neural_network->calculate_outputs(PredictNNptr->data);
         ERL_NIF_TERM prediction = nifpp::makeTensor2D(env, calculate_outputs);
         if(enif_send(NULL,&(PredictNNptr->pid), env,prediction)){
             printf("enif_send succeed prediction\n");
          }
         else printf("enif_send failed\n");
         //delete PredictNNptr;
         return 0;
}


static ERL_NIF_TERM predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         std::shared_ptr<PredictNN> PredictNNptr = std::make_shared<PredictNN>();
         //long int mid;
         //Eigen::Tensor<float,2> data;
         ErlNifPid pid;
         
         enif_self(env, &pid);
         PredictNNptr->pid = pid;

         //opennnBridgeController *s = s->GetInstance();
        
         
         nifpp::get_throws(env, argv[0], PredictNNptr->mid); // get model id
         nifpp::getTensor2D(env,argv[1], PredictNNptr->data); // get data for prediction
         //get neural network from singelton         
         //std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(mid); 
         //Tensor< float, 2 > calculate_outputs =  neural_network->calculate_outputs(data);
        
         //ERL_NIF_TERM prediction = nifpp::makeTensor2D(env, calculate_outputs);
          
         //if(enif_send(NULL,&(pid), env,prediction)){
         //    printf("enif_send succeed prediction\n");
         //}
         //else printf("enif_send failed\n");
         int res = enif_thread_create((char*)"trainModule", &(PredictNNptr->tid), PredictFun, PredictNNptr.get(), 0);
         
         return enif_make_string(env, "end PREDICT mode", ERL_NIF_LATIN1);

         
     //return enif_make_int(env,0);

}  //end PREDICT mode



static ERL_NIF_TERM trainn_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
            
          ERL_NIF_TERM train_time;
          // Start timer for the train
          high_resolution_clock::time_point start = high_resolution_clock::now();
            
         //std::shared_ptr<TrainNN> TrainNNptr = std::make_shared<TrainNN>();
         std::shared_ptr<TrainNN> TrainNNptr = std::make_shared<TrainNN>();
         TrainNNptr->start_time = start;
         
        try{
         nifpp::get_throws(env, argv[0],TrainNNptr->mid); // model id
         nifpp::get_throws(env, argv[1],TrainNNptr->optimization_method);
         nifpp::get_throws(env, argv[2],TrainNNptr->lose_method);
         nifpp::get_throws(env, argv[3],TrainNNptr->learning_rate);
         nifpp::getTensor2D(env,argv[4],TrainNNptr->data);
         //nifpp::get_throws(env, argv[5],TrainNNptr->display);
         
         ErlNifPid pid;
         enif_self(env, &pid);
         TrainNNptr->pid = pid;
         //std::out << "learning_rate : "<< std::endl;
         //std::out << TrainNNptr->learning_rate << std::endl;
        }
        catch(...){
           return enif_make_string(env, "catch - get data from erlang", ERL_NIF_LATIN1);
        }       
         
         int res = enif_thread_create((char*)"trainModule", &(TrainNNptr->tid), trainFun, TrainNNptr.get(), 0);

         return enif_make_string(env, "end comunication", ERL_NIF_LATIN1);
        

}  //end trainn_nif






static ErlNifFunc nif_funcs[] =
{
    {"create_nif", 6 , create_nif},
    //{"train_nif", 4 , train_nif},
    {"trainn_nif", 5 , trainn_nif},
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
