#pragma once 


#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>




#include "../opennn/opennn/opennn.h"
#include "nifpp.h"

using namespace OpenNN;

static ERL_NIF_TERM get_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         
         long int mid;
         ErlNifPid pid;

         enif_self(env, &pid);

         opennnBridgeController *s = s->GetInstance();

         //get model id
         nifpp::get_throws(env, argv[0], mid); 

         //get neural network from singelton           
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(mid);
         
         //cout << neural_network->get_layers_number() <<std::endl;
         //Index num = neural_network->get_layers_number();

         Tensor< float, 1 > parameters = neural_network->get_parameters();
 
         ERL_NIF_TERM erl_parameters = nifpp::makeTensor1D(env, parameters);
         
        cout << "parameters:" <<std::endl;
        cout << parameters <<std::endl;
    

         if(enif_send(NULL,&(pid), env,erl_parameters)){
             printf("enif_send succeed\n");
         }
         else printf("enif_send failed\n");

         return enif_make_string(env, "end get_weights_nif ", ERL_NIF_LATIN1);

     //return enif_make_int(env,0);

}  //end get_weights_nif 




static ERL_NIF_TERM set_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         
         long int mid;
         ErlNifPid pid;
        
         std::shared_ptr<Eigen::Tensor<float,1>> parameters;

         enif_self(env, &pid);
         opennnBridgeController *s = s->GetInstance();

         //get model id
         nifpp::get_throws(env, argv[0], mid); 
         nifpp::getTensor1D(env,argv[1],parameters);

         //get neural network from singelton           
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(mid);
         
         //cout << neural_network->get_layers_number() <<std::endl;
         //Index num = neural_network->get_layers_number();

         neural_network->set_parameters(*parameters);
 
         return enif_make_string(env, "end set_weights_nif ", ERL_NIF_LATIN1);

     //return enif_make_int(env,0);

}  //end set_weights_nif 

 /*
         //get weitghts test
         std::cout << "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" << std::endl; 
         std::cout << parameters2 << std::endl;
         std::cout << "bbbb" << std::endl; 
         //end 
         */
