#pragma once 

//#include <iostream>
#include <vector>
#include <string>
#include "ModelParams.h"
//#include "CustumNN.h"
#include <map>



#include "../opennn/opennn/opennn.h"
#include "nifppEigenExtensions.h"

using namespace OpenNN;


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
