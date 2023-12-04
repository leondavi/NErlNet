#include "nerlWorker.h"

using namespace opennn;

namespace nerlnet
{

// ----- NerlWorker -----
    NerlWorker::NerlWorker(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                     int loss_method, int distributed_system_type, std::string &distributed_system_args_str)
    {
        _learning_rate = learning_rate;
        _epochs = epochs;
        _optimizer_type = optimizer_type;
        _loss_method = loss_method;
    }

    NerlWorker::~NerlWorker()
    {
    }

// ----- NerlWorkerOpenNN -----

    NerlWorkerOpenNN::NerlWorkerOpenNN(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                     int loss_method, int distributed_system_type, std::string &distributed_system_args_str) : NerlWorker(model_type, layer_sizes_str, layer_types_list, layers_functionality,
                                                                                                                      learning_rate, epochs, optimizer_type, optimizer_args_str,
                                                                                                                      loss_method, distributed_system_type, distributed_system_args_str)
    {
        generate_opennn_neural_network();
    }

    std::shared_ptr<NerlLayer> NerlWorkerOpenNN::parse_layers_input(std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality)
    {
        std::shared_ptr<iTensor1D> layer_sizes = std::make_shared<iTensor1D>(1);
        return std::make_shared<NerlLayer>(0, layer_sizes, 1);
        // TODO - Ori and Nadav
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

    int NerlWorkerOpenNN::layer_functionality(int layer_functionality, int layer_type)
    {
        int res;
        switch (layer_type)
        {
            // case LAYER_TYPE_DEFAULT:      { res =          break;}
            // case LAYER_TYPE_SCALING:      { res =           break;}
            case LAYER_TYPE_CNN:          { res = translate_activation_function(layer_functionality); break;}
            case LAYER_TYPE_PERCEPTRON:   { res = translate_activation_function(layer_functionality); break;}
            // case LAYER_TYPE_POOLING:      { res =          break;}
            // case LAYER_TYPE_PROBABILISTIC:{ res =      break;}
            // case LAYER_TYPE_LSTM:         { res =  break;}
            // case LAYER_TYPE_RECCURRENT:   { res =           break;}
        }
       return res;
    }


    // TODO - Ori and Nadav - Implement translation functions
    int translate_layer_type(int layer_type)
    {
        int res;
        switch (layer_type)
        {
            case LAYER_TYPE_DEFAULT:      { res = (int)opennn::Layer::Type::Perceptron;          break;}
            case LAYER_TYPE_SCALING:      { res = (int)opennn::Layer::Type::Scaling;             break;}
            case LAYER_TYPE_CNN:          { res = (int)opennn::Layer::Type::Convolutional;       break;}
            case LAYER_TYPE_PERCEPTRON:   { res = (int)opennn::Layer::Type::Perceptron;          break;}
            case LAYER_TYPE_POOLING:      { res = (int)opennn::Layer::Type::Pooling;             break;}
            case LAYER_TYPE_PROBABILISTIC:{ res = (int)opennn::Layer::Type::Probabilistic;       break;}
            case LAYER_TYPE_LSTM:         { res = (int)opennn::Layer::Type::LongShortTermMemory; break;}
            case LAYER_TYPE_RECCURRENT:   { res = (int)opennn::Layer::Type::Recurrent;           break;}
        }
       return res;
    }

    int NerlWorkerOpenNN::translate_activation_function(int activation_function)
    {
       int res;
       switch (activation_function)
       {
       case ACTIVATION_THRESHOLD:    { res = (int)opennn::PerceptronLayer::ActivationFunction::Threshold;                break;}
       case ACTIVATION_SIGN:         { res = (int)opennn::PerceptronLayer::ActivationFunction::SymmetricThreshold;       break;}
       case ACTIVATION_LOGISTIC:     { res = (int)opennn::PerceptronLayer::ActivationFunction::Logistic;                 break;}
       case ACTIVATION_TANH:         { res = (int)opennn::PerceptronLayer::ActivationFunction::HyperbolicTangent;        break;}
       case ACTIVATION_LINEAR:       { res = (int)opennn::PerceptronLayer::ActivationFunction::Linear;                   break;}
       case ACTIVATION_RELU:         { res = (int)opennn::PerceptronLayer::ActivationFunction::RectifiedLinear;          break;}
       case ACTIVATION_ELU:          { res = (int)opennn::PerceptronLayer::ActivationFunction::ExponentialLinear;        break;}
       case ACTIVATION_SELU:         { res = (int)opennn::PerceptronLayer::ActivationFunction::ScaledExponentialLinear;  break;}
       case ACTIVATION_SOFT_PLUS:    { res = (int)opennn::PerceptronLayer::ActivationFunction::SoftPlus;                 break;}
       case ACTIVATION_SOFT_SIGN:    { res = (int)opennn::PerceptronLayer::ActivationFunction::SoftSign;                 break;}
       case ACTIVATION_HARD_SIGMOID: { res = (int)opennn::PerceptronLayer::ActivationFunction::HardSigmoid;              break;}
       }
       return res;
    }

    int NerlWorkerOpenNN::translate_loss_method(int loss_method)
    {
        int res;
        switch (loss_method)
        {
        case LOSS_METHOD_SSE:        { res = (int)opennn::TrainingStrategy::LossMethod::SUM_SQUARED_ERROR;        break;}
        case LOSS_METHOD_MSE:        { res = (int)opennn::TrainingStrategy::LossMethod::MEAN_SQUARED_ERROR;       break;}
        case LOSS_METHOD_NSE:        { res = (int)opennn::TrainingStrategy::LossMethod::NORMALIZED_SQUARED_ERROR; break;}
        case LOSS_METHOD_MINKOWSKIE: { res = (int)opennn::TrainingStrategy::LossMethod::MINKOWSKI_ERROR;          break;}
        case LOSS_METHOD_WSE:        { res = (int)opennn::TrainingStrategy::LossMethod::WEIGHTED_SQUARED_ERROR;   break;}
        case LOSS_METHOD_CEE:        { res = (int)opennn::TrainingStrategy::LossMethod::CROSS_ENTROPY_ERROR;      break;}
        }
        return res;
    }
    int NerlWorkerOpenNN::translate_optimizer_type(int optimizer_type)
    {   
        int res;
        switch (optimizer_type)
        { 
        case OPTIMIZER_GD:          { res = (int)opennn::TrainingStrategy::OptimizationMethod::GRADIENT_DESCENT;              break;}
        case OPTIMIZER_SGD:         { res = (int)opennn::TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT;   break;}
        case OPTIMIZER_CGD:         { res = (int)opennn::TrainingStrategy::OptimizationMethod::GRADIENT_DESCENT;              break;}
        case OPTIMIZER_QUASINEUTON: { res = (int)opennn::TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD;           break;}
        case OPTIMIZER_LVM:         { res = (int)opennn::TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM; break;}
        case OPTIMIZER_ADAM:        { res = (int)opennn::TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION;    break;}
        }
        return res;
    }

    int NerlWorkerOpenNN::translate_scaling_method(int scaling_method)
    {
        int res;
        switch (scaling_method)
        {
        case SCALING_NONE:    { res = (int)opennn::Scaler::NoScaling;             break;}
        case SCALING_MINMAX:  { res = (int)opennn::Scaler::MinimumMaximum;        break;}
        case SCALING_MEANSTD: { res = (int)opennn::Scaler::MeanStandardDeviation; break;}
        case SCALING_STD:     { res = (int)opennn::Scaler::StandardDeviation;     break;}
        case SCALING_LOG:     { res = (int)opennn::Scaler::Logarithm;             break;}
        }
        return res;
    }

    int NerlWorkerOpenNN::translate_model_type(int model_type)
    {
        int res;
        switch (model_type)
        {
        case MODEL_TYPE_APPROXIMATION: break;
        case MODEL_TYPE_CLASSIFICATION: break;
        case MODEL_TYPE_FORECASTING: break;
        case MODEL_TYPE_ENCODER_DECODER: break;
        case MODEL_TYPE_NN: break;
        case MODEL_TYPE_AUTOENCODER: break;
        case MODEL_TYPE_AE_CLASSIFIER: break;
        }
        return 0;
    }

} // namespace nerlnet