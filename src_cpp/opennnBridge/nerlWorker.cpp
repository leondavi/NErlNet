#include "nerlWorker.h"

namespace nerlnet
{

// ----- NerlWorker -----
    NerlWorker::NerlWorker(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                           float learning_rate, int epochs, int optimizer_type, int loss_method)
    {
        parse_layers_input(layer_sizes_str, layer_types_list, layers_functionality);
        _learning_rate = learning_rate;
        _epochs = epochs;
        _optimizer_type = optimizer_type;
        _loss_method = loss_method;
    }

    NerlWorker::~NerlWorker()
    {
    }

    void NerlWorker::parse_layers_input(std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality)
    {
        // TODO - Ori and Nadav
    }

// ----- NerlWorkerOpenNN -----

    NerlWorkerOpenNN::NerlWorkerOpenNN(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                                       float learning_rate, int epochs, int optimizer_type, int loss_method) : NerlWorker(model_type, layer_sizes_str, layer_types_list, layers_functionality,
                                                                                                                        learning_rate, epochs, optimizer_type, loss_method)
    {
        generate_opennn_neural_network();
    }

    NerlWorkerOpenNN::~NerlWorkerOpenNN()
    {
    }

    void NerlWorkerOpenNN::get_opennn_neural_network_ptr(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr)
    {
        neural_network_ptr = _neural_network;
    }

    void NerlWorkerOpenNN::generate_opennn_neural_network()
    {
        // TODO - Ori and Nadav
    }

    // TODO - Ori and Nadav - Implement translation functions
    int translate_layer_type(int layer_type)
    {
        return 0;
    }

    int translate_layer_functionality(int layer_functionality)
    {
        return 0;
    }

    int translate_activation_function(int activation_function)
    {
        return 0;
    }

    int translate_loss_method(int loss_method)
    {
        return 0;
    }
    int translate_optimizer_type(int optimizer_type)
    {
        return 0;
    }

    int translate_scaling_method(int scaling_method)
    {
        return 0;
    }

    int translate_model_type(int model_type)
    {
        return 0;
    }

} // namespace nerlnet