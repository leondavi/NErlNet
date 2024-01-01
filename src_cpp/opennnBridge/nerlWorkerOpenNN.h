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

    void generate_opennn_neural_network();
    std::shared_ptr<opennn::NeuralNetwork> get_neural_network_ptr() { return _neural_network; };

    private:

    std::shared_ptr<opennn::NeuralNetwork> _neural_network;
    std::shared_ptr<opennn::TrainingStrategy> _training_strategy;


    // neural network generator functions
    void generate_opennn_project(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);
    void generate_custom_model_nn(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);
    void generate_custom_model_aec(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);
    void generate_custom_model_ae(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);
    void generate_custom_model_lstm(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);
    void generate_custom_model_recurrent(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);


    // translation functions
    int layer_functionality(int layer_functionality, int layer_type);
    int translate_layer_type(int layer_type);
    opennn::PerceptronLayer::ActivationFunction translate_activation_function(int activation_function);
    int translate_activation_function_int(int activation_function);
    int translate_loss_method(int loss_method);
    int translate_optimizer_type(int optimizer_type);
    int translate_scaling_method_int(int scaling_method);
    opennn::Scaler translate_scaling_method(int scaling_method);
    int translate_unscaling_method_int(int unscaling_method);
    opennn::Scaler translate_unscaling_method(int scaling_method);
    opennn::PoolingLayer::PoolingMethod translate_pooling_method(int pooling_method);
    int translate_pooling_method_int(int pooling_method);
    int translate_model_type(int model_type, int &custom_model);
};

} // namespace nerlnet