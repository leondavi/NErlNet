#pragma once 


#include <vector>
#include <string>
#include "ModelParams.h"
#include <map>
#include "../opennn/opennn/opennn.h"
#include "nifpp.h"

using namespace OpenNN;

 //send NN wieghts to erlang worker

static ERL_NIF_TERM get_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         
         long int mid;
         ErlNifPid pid;

         enif_self(env, &pid);

         opennnBridgeController *s = s->GetInstance();

         //get model id
         nifpp::get_throws(env, argv[0], mid); 

         //get neural network from singelton           
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s-> getModelPtr(mid);
         
         Tensor< float, 1 > parameters = neural_network->get_parameters();
        //  Tensor<Tensor<type, 1>, 1> parameters2 = get_trainable_layers_parameters(parameters);
 
         ERL_NIF_TERM erl_parameters = nifpp::makeTensor1D(env, parameters);
         
        cout << "sending parameters with size of:" ;
        cout << parameters.size() <<std::endl;
        // cout << parameters <<std::endl;
    
        try{
         if(enif_send(NULL,&(pid), env,erl_parameters)){
             printf("enif_send succeed\n");
         }
         else printf("enif_send failed\n");
        }
        catch(...){ cout << "/////////////////////////////////////////#################" <<std::endl;}
         return enif_make_string(env, "end get_weights_nif ", ERL_NIF_LATIN1);


}  //end get_weights_nif 



static ERL_NIF_TERM set_weights_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){ 
         // return enif_make_string(env, "end set_weights_nif ", ERL_NIF_LATIN1);
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
         int NN_parameters_number = neural_network->get_parameters_number();
         int new_parameters_number = parameters->size();
         try{
         //cout << neural_network->get_layers_number() <<std::endl;
         //Index num = neural_network->get_layers_number();
         if(NN_parameters_number == new_parameters_number){
         cout << "111111111" <<std::endl;
            neural_network->set_parameters(*parameters);
         cout << "222222222" <<std::endl;
         }
         else{
            cout << "error update NN parameters" <<  std::endl;
            cout << "NN_parameters_number: " << NN_parameters_number << std::endl;
            cout << "new_parameters_number: " << new_parameters_number << std::endl;
         }
         return enif_make_string(env, "end set_weights_nif ", ERL_NIF_LATIN1);
       
}  //end set_weights_nif 


