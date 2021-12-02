#pragma once 


#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>



#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"
#include "nifpp.h"

using namespace OpenNN;

static ERL_NIF_TERM get_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         
         long int mid;
         opennnBridgeController *s = s->GetInstance();

         // get model id
         nifpp::get_throws(env, argv[0], mid); 

         //get neural network from singelton           
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(mid);
         
         cout << neural_network->get_layers_number() <<std::endl;
      
         Index num = neural_network->get_layers_number();
        
         /*
         //get weitghts test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         Tensor< float, 1 > parameters2 = neural_network->get_parameters();
         std::cout << parameters2 << std::endl;
         std::cout << "bbbb" << std::endl; 
         //end 
         */
         return enif_make_string(env, "end get_weights_nif ", ERL_NIF_LATIN1);

     //return enif_make_int(env,0);

}  //end get_weights_nif 
