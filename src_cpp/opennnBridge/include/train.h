#pragma once 

//#include <iostream>
//#include "convert.h"
#include "nifpp.h"
#include <erl_nif.h>
#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>

//#include "Eigen/Core"
//#include "unsupported/Eigen/CXX11/Tensor"
#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"
//#include "bridgeController.h"



using namespace OpenNN;

static void* trainFun(void *arg){

    TrainParams* trainPtr = (TrainParams*)arg;
    //double loss_val;
    ErlNifEnv *env = enif_alloc_env();
    //ERL_NIF_TERM loss_val_term;

    // Get the singleton instance
    opennnBridgeController *s = s->GetInstance();

    // Get the model from the singleton
    std::shared_ptr<OpenNN::NeuralNetwork> modelPtr = s-> getModelPtr(trainPtr->_mid);

    // Get the data matrix from the data_label_mat vector and initialize the data matrix.
    MatrixXd data_mat(trainPtr->_rows, trainPtr->_col);

    // Get the label matrix from the data_label_mat vector and initialize the label matrix
    MatrixXd label_mat(trainPtr->_rows, trainPtr->_labels);

    // Convert the vector to a matrix. Native to Eigen
    int FeaturesAndLabels = trainPtr->col+trainPtr->labels; // Number of columns in total
    data_mat = Map<MatrixXd,0, Stride<Dynamic,Dynamic>>(trainPtr->data_label_mat.data(), trainPtr->rows, trainPtr->col,Stride<Dynamic,Dynamic>(1, FeaturesAndLabels));
    label_mat = Map<MatrixXd,0, Stride<Dynamic,Dynamic>>(&trainPtr->data_label_mat[trainPtr->col], trainPtr->rows, trainPtr->labels,Stride<Dynamic,Dynamic>(1, FeaturesAndLabels));
    
    

     // Start the timer for the training in cppSANN
    //auto start = high_resolution_clock::now();

    // Train the model with recieved parameters and get loss value
    //loss_val = modelPtr->train(data_mat,label_mat);                   its impotent

    // Stop the timer and calculate the time took for training
    //auto stop = high_resolution_clock::now();
    //auto duration = duration_cast<microseconds>(stop - start);


    // Check if the loss value is in range and not -NaN
    
    // Send to erlang process the loss value and time
    if(enif_send(NULL,&(trainPtr->pid), env,loss_val_term)){
    #if DEBUG_PREDICT_NIF
            printf("enif_send succeed\n");
    #endif
    }
    else
        printf("enif_send failed\n");

    // Frees all terms in an environment and clears it for reuse.
    enif_clear_env(env);

    delete trainPtr;
#if DEBUG_PREDICT_NIF
    printf("Finish train fun thread in the nif.\n");
#endif

    return 0;
}
