#pragma once

#include "../opennn/opennn/opennn.h"
#include "nerlLayer.h"
#include <vector>


namespace nerlnet
{
/*
LayerSizes Input: std::vector<std::string> layerSizes 16x16k5s2,8,4,3,2
*/

class NerlWorker
{
    public:

    NerlWorker(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
               float learning_rate, int epochs, int optimizer_type, int loss_method );
    ~NerlWorker();
    std::shared_ptr<NerlLayer> get_layer(std::shared_ptr<NerlLayer> next_layer);

    void parse_layers_input(std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality);

    float get_learning_rate() { return _learning_rate; };
    int get_epochs() { return _epochs; };
    int get_optimizer_type() { return _optimizer_type; };
    int get_loss_method() { return _loss_method; };

    private:

    std::vector<NerlLayer> _layers;
    float _learning_rate;
    int _epochs;
    int _optimizer_type;
    int _loss_method;

};

class NerlWorkerOpenNN : public NerlWorker
{
    public:

    NerlWorkerOpenNN(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, int loss_method);
    ~NerlWorkerOpenNN();

    void get_opennn_neural_network_ptr(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr);
    void generate_opennn_neural_network();

    private:
    std::shared_ptr<opennn::NeuralNetwork> _neural_network;

    // translation functions
    int translate_layer_type(int layer_type);
    int translate_layer_functionality(int layer_functionality);
    int translate_activation_function(int activation_function);
    int translate_loss_method(int loss_method);
    int translate_optimizer_type(int optimizer_type);
    int translate_scaling_method(int scaling_method);
    int translate_model_type(int model_type);
};

} // namespace nerlnet