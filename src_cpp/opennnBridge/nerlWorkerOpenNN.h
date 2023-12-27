#pragma once

#include "../opennn/opennn/opennn.h"
#include "../common/nerlWorker.h"
#include "eigenTensorTypes.h"
#include "worker_definitions_ag.h"

namespace nerlnet
{

class NerlWorkerOpenNN : public NerlWorker
{
    public:

    NerlWorkerOpenNN(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                     int loss_method, int distributed_system_type, std::string &distributed_system_args_str);
    ~NerlWorkerOpenNN();

    void get_opennn_neural_network_ptr(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);
    void generate_opennn_neural_network();

    private:
    //std::shared_ptr<nerlnet::NerlLayer> _first_layer; // linked list of all layers
    std::shared_ptr<opennn::NeuralNetwork> _neural_network;

    // translation functions
    int layer_functionality(int layer_functionality, int layer_type);
    int translate_layer_type(int layer_type);
    int translate_activation_function(int activation_function);
    int translate_loss_method(int loss_method);
    int translate_optimizer_type(int optimizer_type);
    int translate_scaling_method(int scaling_method);
    int translate_model_type(int model_type);
};

} // namespace nerlnet