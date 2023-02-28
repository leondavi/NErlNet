/*****************************************************
 * Authors: David Leon, Ziv Moldavsky, Evgeny Andrachnik
 * 22/7/2022
 * 
 * Copyright (c) 2021 Nerlnet
 *****************************************************/ 

#pragma once 


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
#include "Autoencoder.h"
#include "get_set_weights.h"

#include "nifppEigenExtensions.h"

using namespace OpenNN;

#define DEBUG_CREATE_NIF 0


struct PredictNN {

    long int mid;
    std::shared_ptr<Eigen::Tensor<float,2>> data;
    ErlNifPid pid;
    ErlNifTid tid;
};


static void* PredictFun(void* arg){ 

         PredictNN* PredictNNptr = reinterpret_cast<PredictNN*>(arg);
         ERL_NIF_TERM prediction;
         int EAC_prediction; 
         ErlNifEnv *env = enif_alloc_env();    
         opennnBridgeController *s = s->GetInstance();
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(PredictNNptr->mid);
               //   cout << "222222222222" << endl;

         int modelType = s->getModelType(PredictNNptr->mid); 
         std::shared_ptr<Eigen::Tensor<float,2>> calculate_res = std::make_shared<Eigen::Tensor<float,2>>();
         *calculate_res = neural_network->calculate_outputs( *(PredictNNptr->data));
            //   cout << "33333333333333" << endl;

         if(modelType == E_AEC){
             
             std::shared_ptr<AutoencoderClassifier> Autoencoder_Classifier = std::static_pointer_cast<AutoencoderClassifier>(neural_network);
             Eigen::Tensor<int, 1> predictRes  = Autoencoder_Classifier->predict(PredictNNptr->data);
             prediction = nifpp::makeTensor1D(env, (predictRes));
         }
         else
             prediction = nifpp::makeTensor2D(env, *calculate_res);
             
                  // cout << "44444444444" << endl;
         if(enif_send(NULL,&(PredictNNptr->pid), env, prediction)){
            // printf("enif_send succeed prediction\n");
          }
         else printf("enif_send failed\n");
         return 0;
}


static ERL_NIF_TERM predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 

         std::shared_ptr<PredictNN> PredictNNptr = std::make_shared<PredictNN>();

         ErlNifPid pid;
         
         enif_self(env, &pid);
         PredictNNptr->pid = pid;

         nifpp::get_throws(env, argv[0], PredictNNptr->mid); // get model id
         nifpp::getTensor2D(env,argv[1], PredictNNptr->data); // get data for prediction
 
         int res = enif_thread_create((char*)"trainModule", &(PredictNNptr->tid), PredictFun, PredictNNptr.get(), 0);
         return enif_make_string(env, "end PREDICT mode", ERL_NIF_LATIN1);

}  //end PREDICT mode



static ERL_NIF_TERM trainn_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
           
          ERL_NIF_TERM train_time;
          // Start timer for the train
          high_resolution_clock::time_point start = high_resolution_clock::now();
            
         std::shared_ptr<TrainNN> TrainNNptr = std::make_shared<TrainNN>();
         TrainNNptr->start_time = start;
         
        try{
         nifpp::get_throws(env, argv[0],TrainNNptr->mid); // model id
         nifpp::get_throws(env, argv[1],TrainNNptr->optimization_method);
         nifpp::get_throws(env, argv[2],TrainNNptr->lose_method);
         nifpp::get_throws(env, argv[3],TrainNNptr->learning_rate);
         nifpp::getTensor2D(env,argv[4],TrainNNptr->data);

         ErlNifPid pid;
         enif_self(env, &pid);
         TrainNNptr->pid = pid;
        }
        catch(...){
           return enif_make_string(env, "catch - get data from erlang", ERL_NIF_LATIN1);
        }  

         try{
         int res = enif_thread_create((char*)"trainModule", &(TrainNNptr->tid), trainFun, TrainNNptr.get(), 0);
         }
         catch(...){
            cout << "catch in enif_thread_create " << endl;
         }

         return enif_make_string(env, "end comunication", ERL_NIF_LATIN1);
}  //end trainn_nif


bool is_big_endian(void)
{
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01020304};

    return bint.c[0] == 1;
}

static ERL_NIF_TERM encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 

    #if DEBUG_ENCODE
        std::cout << "Start the encode_nif." << '\n';
    #endif

    bool isEndian = is_big_endian();
    if (isEndian)
    {
        std::cout << "\nThe system is Big Endian: " << std::endl;
    }
    else
    {
        std::cout << "\nThe system is Little Endian: " << std::endl;
    }
    

    int NumOfBytes{};

    // Get NumOfBytes (int) from erlang term
    if (!enif_get_int(env, argv[1], &NumOfBytes)) {
        return enif_make_badarg(env);
    }

    union {
        int receiveInt;
        double receivedDouble;
        char arrayOfChars[sizeof(double)];
    } receivedNum;


    if (enif_get_double(env, argv[0], &receivedNum.receivedDouble))
    {
        #if DEBUG_ENCODE
            std::cout << "Its a double" << std::endl;
        #endif

        return enif_make_string_len(env, (char*)(&receivedNum.arrayOfChars),NumOfBytes, ERL_NIF_LATIN1);
    } 
    else if (enif_get_int(env, argv[0], &receivedNum.receiveInt)) 
    {
        #if DEBUG_ENCODE
            std::cout << "Its an integer" << std::endl;
        #endif

        return enif_make_string_len(env, (char*)(&receivedNum.arrayOfChars),NumOfBytes, ERL_NIF_LATIN1);
    } 
    else
    {
        return enif_make_atom(env, "Not_a_number");
    }

    #if DEBUG_ENCODE
        std::cout << "Print array_of_chars:" << std::endl;
        for(int k = 0; k < sizeof(double); k++)
        {
            std::cout << (int)receivedNum.arrayOfChars[k] << std::endl;
        }
    #endif

    return enif_make_atom(env, "Finished encode NIF not as expected");

}  


static ERL_NIF_TERM decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 

    #if DEBUG_DECODE
        std::cout << "Start the decode_nif." << '\n';
    #endif

    int NumOfBytes{};

    // Get NumOfBytes (int) from erlang term
    if (!enif_get_int(env, argv[1], &NumOfBytes)) {
        return enif_make_badarg(env);
    }

    union {
        int receiveInt;
        double receivedDouble;
        unsigned char arrayOfChars[sizeof(double)];
        
    } receivedString;

    //char* buf = (char*) enif_alloc(NumOfBytes + 1);
    
    if (!enif_get_string(env, argv[0], (char*)receivedString.arrayOfChars, NumOfBytes+1, ERL_NIF_LATIN1)) {
        enif_free(receivedString.arrayOfChars);
        return enif_make_badarg(env);
    }

    #if DEBUG_DECODE
        std::cout << "Print array_of_chars:" << std::endl;
        for(int k = 0; k < sizeof(receivedString.arrayOfChars); k++)
        {
            std::cout << (int)receivedString.arrayOfChars[k] << std::endl;
        }
        //std::cout << receivedString.receivedDouble << std::endl;
        //std::cout << receivedString.receiveInt << std::endl;
    #endif

    if(NumOfBytes == 8)
    {
        return enif_make_double(env, receivedString.receivedDouble);
    }
    else if (NumOfBytes == 4)
    {
        return enif_make_int(env, receivedString.receiveInt);
    }

    return enif_make_atom(env, "Finished decode NIF not as expected");
}  


static ErlNifFunc nif_funcs[] =
{
    {"create_nif", 6 , create_nif},
    {"trainn_nif", 5 , trainn_nif},
    {"predict_nif", 2 , predict_nif},
    {"get_weights_nif",1, get_weights_nif},
    {"set_weights_nif",2, set_weights_nif},
    {"encode",2, encode_nif},
    {"decode",2, decode_nif}
};


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
