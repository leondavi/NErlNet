#pragma once

#include <Logger.h>
#include "nerlWorkerFunc.h"
#include "nerlLayer.h"

namespace nerlnet
{
    class NerlWorker  // every child class of NerlWorker must implement the same constrctor signature 
{
    public:

    NerlWorker(int model_type, std::string &model_args_str, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                     int loss_method, std::string &loss_args_str, int distributed_system_type, std::string &distributed_system_args_str);
    ~NerlWorker();

    std::shared_ptr<NerlLayer> parse_layers_input(std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality);

    float get_learning_rate() { return _learning_rate; };
    int get_epochs() { return _epochs; };
    int get_optimizer_type() { return _optimizer_type; };
    int get_loss_method() { return _loss_method; };
    int get_distributed_system_type() { return _distributed_system_type; };
    virtual std::shared_ptr<std::vector<int>> get_distributed_system_train_labels_count() {LogError<<"Distributed System Weighted Avg count label is unsupported";
                                                                                          throw("Distributed System Weighted Avg count label is unsupported");} // counts the number of each label appears in the training set for weighted average

    protected:

    std::shared_ptr<NerlLayer> _nerl_layers_linked_list;
    int _model_type;
    std::string _model_args_str;
    int _distributed_system_type;
    float _learning_rate;
    int _epochs;
    int _optimizer_type;
    int _loss_method;
    std::string _loss_args_str;
    std::string _distributed_system_args_str;
    std::shared_ptr<std::vector<int>> _train_labels_count; // accumulates the number of each label in the training set


    private:

};
}