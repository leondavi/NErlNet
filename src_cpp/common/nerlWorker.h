#pragma once

#include "nerlWorkerFunc.h"
#include "nerlLayer.h"

namespace nerlnet
{
    class NerlWorker  // every child class of NerlWorker must implement the same constrctor signature 
{
    public:

    NerlWorker(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                     int loss_method, int distributed_system_type, std::string &distributed_system_args_str);
    ~NerlWorker();

    std::shared_ptr<NerlLayer> parse_layers_input(std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality);

    float get_learning_rate() { return _learning_rate; };
    int get_epochs() { return _epochs; };
    int get_optimizer_type() { return _optimizer_type; };
    int get_loss_method() { return _loss_method; };

    protected:

    std::shared_ptr<NerlLayer> _nerl_layers_linked_list;
    int _model_type;
    int _distributed_system_type;
    float _learning_rate;
    int _epochs;
    int _optimizer_type;
    int _loss_method;
    std::string _distributed_system_args_str;

    private:

};
}