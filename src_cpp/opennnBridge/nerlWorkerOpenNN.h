#pragma once

#include <cassert>
#include <Logger.h>

#include "../opennn/opennn/opennn.h"
#include "../common/nerlLayer.h"
#include "../common/nerlWorker.h"
#include "eigenTensorTypes.h"
#include "worker_definitions_ag.h"
#include "ae_red.h"

#define TRAINING_STRATEGY_SET_DISPLAY_ON   1
#define TRAINING_STRATEGY_SET_DISPLAY_OFF  0

namespace nerlnet
{

class NerlWorkerOpenNN : public NerlWorker
{
public:
    NerlWorkerOpenNN(WorkerParams worker_params,
                     int distributed_system_type,
                     std::string distributed_system_args_str);
    ~NerlWorkerOpenNN() override;

    void generate_opennn_neural_network();
    void generate_training_strategy();

    std::shared_ptr<opennn::NeuralNetwork> get_neural_network_ptr() { return _neural_network_ptr; };
    std::shared_ptr<opennn::TrainingStrategy> get_training_strategy_ptr() { return _training_strategy_ptr; };
    std::shared_ptr<opennn::DataSet> get_data_set() { return _data_set; };
    void post_training_process(fTensor2DPtr  TrainDataNNptr);
    void post_predict_process(fTensor2DPtr &result_ptr, fTensor2DPtr &predictData);
    void get_result_calc(fTensor2DPtr calculate_res,int num_of_samples,int inputs_number,fTensor2DPtr predictData);
    void set_optimization_method(int optimizer_type ,int learning_rate);
    void set_loss_method(int loss_method);
    void set_learning_rate(float learning_rate);
    void set_epochs(int epochs);
    void set_dataset(std::shared_ptr<opennn::DataSet> data_set,fTensor2DPtr TrainDataNNptr);
    std::shared_ptr<opennn::DataSet> get_dataset_ptr() { return _data_set; };
    
    std::shared_ptr<std::vector<int>> get_distributed_system_train_labels_count() override;

    void perform_training();
    fTensor2DPtr get_loss_nerltensor(); // this is the last calculated loss by perform training


private:
    const std::string &get_required_param(const std::string &key) const;
    std::string get_optional_param(const std::string &key, const std::string &fallback = "") const;
    int get_required_int_param(const std::string &key) const;
    float get_required_float_param(const std::string &key) const;
    std::shared_ptr<NerlLayer> parse_layers_input(const std::string &layer_sizes_str,
                                                 const std::string &layer_types_list,
                                                 const std::string &layers_functionality);

    std::shared_ptr<opennn::NeuralNetwork> _neural_network_ptr;
    std::shared_ptr<opennn::TrainingStrategy> _training_strategy_ptr;
    std::shared_ptr<opennn::DataSet> _data_set;

    fTensor2DPtr _aec_data_set;
    fTensor2DPtr _aec_all_loss_values;
    std::shared_ptr<AeRed> _ae_red_ptr;

    // training vars
    double _last_loss;

    std::shared_ptr<NerlLayer> _nerl_layers_linked_list;
    int _model_type{0};
    std::string _model_args_str;
    float _learning_rate{0.0F};
    int _epochs{0};
    int _optimizer_type{0};
    int _loss_method{0};
    std::string _loss_args_str;
    std::string _optimizer_args_str;
    
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
    opennn::TrainingStrategy::LossMethod translate_loss_method(int loss_method);
    int translate_loss_method_int(int loss_method);
    opennn::TrainingStrategy::OptimizationMethod translate_optimizer_type(int optimizer_type);
    int translate_optimizer_type_int(int optimizer_type);
    int translate_scaling_method_int(int scaling_method);
    opennn::Scaler translate_scaling_method(int scaling_method);
    int translate_unscaling_method_int(int unscaling_method);
    opennn::Scaler translate_unscaling_method(int scaling_method);
    opennn::PoolingLayer::PoolingMethod translate_pooling_method(int pooling_method);
    opennn::ProbabilisticLayer::ActivationFunction translate_probabilistic_activation_function(int activation_function);
    opennn::LossIndex::RegularizationMethod parse_regularization_loss_args(const std::string &loss_args_str);

    int translate_pooling_method_int(int pooling_method);
    int translate_model_type(int model_type, int &custom_model);
};

} // namespace nerlnet