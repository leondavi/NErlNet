#include "nerlWorkerOpenNN.h"

using namespace opennn;

namespace nerlnet
{
// ----- NerlWorkerOpenNN -----

    NerlWorkerOpenNN::NerlWorkerOpenNN(int model_type, std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                     int loss_method, int distributed_system_type, std::string &distributed_system_args_str) : NerlWorker(model_type, layer_sizes_str, layer_types_list, layers_functionality,
                                                                                                                      learning_rate, epochs, optimizer_type, optimizer_args_str,
                                                                                                                      loss_method, distributed_system_type, distributed_system_args_str)
    {
        _neural_network_ptr = std::make_shared<opennn::NeuralNetwork>();
        generate_opennn_neural_network();
        _training_strategy_ptr = std::make_shared<opennn::TrainingStrategy>();
        generate_training_strategy();
        //TODO Ori and Nadav - implement training strategy (loss method, optimizer type, epochs, only sgd and adam)
    }

    NerlWorkerOpenNN::~NerlWorkerOpenNN()
    {

    }

    /**
     * @brief generate the training strategy for the opennn neural network
     * this function doesn't set the data pointer of the training strategy and doesn't call to the train function
    **/
    void NerlWorkerOpenNN::generate_training_strategy()
    {
     _training_strategy_ptr->set_neural_network_pointer(_neural_network_ptr.get()); // Neural network must be defined at this point
    _training_strategy_ptr->set_optimization_method((opennn::TrainingStrategy::OptimizationMethod) translate_optimizer_type_int(_optimizer_type));
     _training_strategy_ptr->set_loss_method((opennn::TrainingStrategy::LossMethod) translate_loss_method_int(_loss_method)); 
     _training_strategy_ptr->set_maximum_epochs_number(_epochs); 
     _training_strategy_ptr->set_display(TRAINING_STRATEGY_SET_DISPLAY_OFF); // remove opennn training strategy prints
    }

    void NerlWorkerOpenNN::set_optimization_method(int optimizer_type,int learning_rate){
        assert((_training_strategy_ptr->has_neural_network(), "NerlWorkerOpenNN::set_optimization_method - neural network pointer is null"));
        _optimizer_type = optimizer_type;
        _training_strategy_ptr->set_optimization_method(translate_optimizer_type(optimizer_type));
        switch(_optimizer_type){
            case OPTIMIZER_GD:
            {
                break; //No implementation for learning rate in GD
            }
            case OPTIMIZER_SGD:
            {
                _training_strategy_ptr->get_stochastic_gradient_descent_pointer()->set_initial_learning_rate(learning_rate);
                break;
            }
            case OPTIMIZER_CGD:
            {
                break; // No learning rate for CGD
            }
            case OPTIMIZER_QUASINEUTON:
            {
                break; //No learning rate for Quasi Newton
            }
            case OPTIMIZER_LVM:
            {
                break; //No learning rate for LVM
            }
            case OPTIMIZER_ADAM:
            {
                _training_strategy_ptr->get_adaptive_moment_estimation_pointer()->set_initial_learning_rate(learning_rate);
                break;
            }
        }
    }

    void NerlWorkerOpenNN::set_loss_method(int loss_method){
        assert((_training_strategy_ptr->has_neural_network(), "NerlWorkerOpenNN::set_loss_method - neural network pointer is null"));
        _loss_method = loss_method;
        _training_strategy_ptr->set_loss_method(translate_loss_method(loss_method));
    }

    void NerlWorkerOpenNN::generate_opennn_neural_network()
    {
        int nerlnet_custom_model; // defines if this is a nerlnet custom project or an opennn project
        int model_type_tr = translate_model_type(_model_type, nerlnet_custom_model);
        switch(nerlnet_custom_model)
        {
            case false:
            {
                generate_opennn_project(_neural_network_ptr);
                break;
            }
            case true:
            {
                switch(model_type_tr)
                {
                    case MODEL_TYPE_NN:
                    {
                        generate_custom_model_nn(_neural_network_ptr);
                        break;
                    }
                    case MODEL_TYPE_AUTOENCODER: //TODO
                    {
                        generate_custom_model_ae(_neural_network_ptr);
                        break;
                    }
                    case MODEL_TYPE_AE_CLASSIFIER: //TODO
                    {
                        generate_custom_model_aec(_neural_network_ptr);
                        break;
                    }
                    // case MODEL_TYPE_LSTM:
                    // {
                    //     generate_custom_model_lstm(_neural_network_ptr);
                    //     break;
                    // }
                    // case MODEL_TYPE_RECURRENT:
                    // {
                    //     generate_custom_model_recurrent(_neural_network_ptr);
                    //     break;
                    // }
                }
                break;
            }
        }
    }

    void NerlWorkerOpenNN::generate_opennn_project(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr)
    {
        // TODO Ori and Nadav - implement

        switch (_model_type)
        {
        case MODEL_TYPE_APPROXIMATION:
           neural_network_ptr->set_project_type(opennn::NeuralNetwork::ProjectType::Approximation);
            break;
        case MODEL_TYPE_CLASSIFICATION:
            neural_network_ptr->set_project_type(opennn::NeuralNetwork::ProjectType::Classification);
            break;
        case MODEL_TYPE_FORECASTING:
            neural_network_ptr->set_project_type(opennn::NeuralNetwork::ProjectType::Forecasting);
            break;
        default:
            break;
        }

    }

    void NerlWorkerOpenNN::generate_custom_model_nn(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr)
    {        
        shared_ptr<NerlLayer> curr_layer = _nerl_layers_linked_list;
        while(curr_layer)
        {
            int layer_type = curr_layer->get_layer_type();
            switch(layer_type)
            {
                case LAYER_TYPE_CNN: 
                {
                    break;
                }
                case LAYER_TYPE_LSTM:
                {
                    break;
                }
                case LAYER_TYPE_RECCURRENT:
                {
                    break;
                }
                case LAYER_TYPE_DEFAULT:
                {
                    // continue to perceptron case
                }
                case LAYER_TYPE_PERCEPTRON:
                {
                    if (curr_layer->is_first())
                    {
                       LogError("NerlWorkerOpenNN::generate_custom_model_nn - PERCEPTRON cannot be first layer");
                       throw std::invalid_argument("NerlWorkerOpenNN::generate_custom_model_nn - PERCEPTRON cannot be first layer");
                    }
                    std::shared_ptr<NerlLayer> prev_layer = curr_layer->get_prev_layer_ptr();
                    int prev_layer_size = prev_layer->get_dim_size(DIM_X_IDX);
                    int layer_size_curr = curr_layer->get_dim_size(DIM_X_IDX);
                    int get_layer_functionality = curr_layer->get_layer_functionality();
                    PerceptronLayer* newLayer =  new opennn::PerceptronLayer(prev_layer_size, layer_size_curr);
                    newLayer->set_activation_function(translate_activation_function(get_layer_functionality));
                    neural_network_ptr->add_layer(newLayer);
                    break;
                }
                case LAYER_TYPE_SCALING:
                {
               //     std::vector<int> layer_dims_vec;
                 //   curr_layer->get_layer_size(layer_dims_vec);
                    int layer_size_curr = curr_layer->get_dim_size(DIM_X_IDX);
                    int get_layer_functionality = curr_layer->get_layer_functionality();
                    ScalingLayer* newLayer = new opennn::ScalingLayer(layer_size_curr);
                    newLayer->set_scalers(translate_scaling_method(get_layer_functionality));
                    neural_network_ptr->add_layer(newLayer);
                    break;

                }
                  case LAYER_TYPE_UNSCALING:
                {
                    std::vector<int> layer_dims_vec;
                    curr_layer->get_layer_size(layer_dims_vec);
                    int layer_size_curr = curr_layer->get_dim_size(DIM_X_IDX);
                    int get_layer_functionality = curr_layer->get_layer_functionality();
                    UnscalingLayer* newLayer = new opennn::UnscalingLayer(layer_size_curr);
                    newLayer->set_scalers(translate_unscaling_method(get_layer_functionality));
                    neural_network_ptr->add_layer(newLayer);
                    break;

                }
                  case LAYER_TYPE_PROBABILISTIC:
                {
                      if (curr_layer->is_first())
                    {
                       LogError("NerlWorkerOpenNN::generate_custom_model_nn - PROBABILISTIC cannot be first layer");
                       throw std::invalid_argument("NerlWorkerOpenNN::generate_custom_model_nn - PROBABILISTIC cannot be first layer");
                    }
                    std::shared_ptr<NerlLayer> prev_layer = curr_layer->get_prev_layer_ptr();
                    int prev_layer_size = prev_layer->get_dim_size(DIM_X_IDX);
                    int layer_size_curr = curr_layer->get_dim_size(DIM_X_IDX);
                    std::vector<int> layer_dims_vec;
                    curr_layer->get_layer_size(layer_dims_vec); //TODO remove from all layers                 
                    int get_layer_functionality = curr_layer->get_layer_functionality();
                    ProbabilisticLayer* newLayer =  new opennn::ProbabilisticLayer(prev_layer_size, layer_size_curr);
                    newLayer->set_activation_function((opennn::ProbabilisticLayer::ActivationFunction)translate_activation_function(get_layer_functionality));
                    neural_network_ptr->add_layer(newLayer);
                    break;
                }
            }  
            
            curr_layer = curr_layer->get_next_layer_ptr();
        }
    }

    void NerlWorkerOpenNN::generate_custom_model_aec(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr)
    {
        // TODO Guy - implement
    }

    void NerlWorkerOpenNN::generate_custom_model_ae(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr)
    {
        // TODO Guy - implement
    }

    void NerlWorkerOpenNN::generate_custom_model_lstm(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr)
    {
        // TODO Ori and Nadav - implement
    }

    void NerlWorkerOpenNN::generate_custom_model_recurrent(std::shared_ptr<opennn::NeuralNetwork> &neural_network_ptr)
    {
        // TODO Ori and Nadav - implement
    }

    int NerlWorkerOpenNN::layer_functionality(int layer_functionality, int layer_type)
    {
        int res;
        switch (layer_type)
        {
            case LAYER_TYPE_DEFAULT:      { res = translate_activation_function_int(layer_functionality); break;} // PERCEPTRON
            case LAYER_TYPE_SCALING:      { res = translate_scaling_method_int(layer_functionality);      break;}
            case LAYER_TYPE_CNN:          { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_PERCEPTRON:   { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_POOLING:      { res = translate_pooling_method_int(layer_functionality);      break;}
            case LAYER_TYPE_PROBABILISTIC:{ res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_LSTM:         { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_RECCURRENT:   { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_UNSCALING:    { res = translate_unscaling_method_int(layer_functionality);    break;}
        }
       return res;
    }

    int NerlWorkerOpenNN::translate_layer_type(int layer_type)
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

    int NerlWorkerOpenNN::translate_activation_function_int(int activation_function)
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

  PerceptronLayer::ActivationFunction NerlWorkerOpenNN::translate_activation_function(int activation_function)
    {
    PerceptronLayer::ActivationFunction res;
       switch (activation_function)
       {
       case ACTIVATION_THRESHOLD:    { res = opennn::PerceptronLayer::ActivationFunction::Threshold;                break;}
       case ACTIVATION_SIGN:         { res = opennn::PerceptronLayer::ActivationFunction::SymmetricThreshold;       break;}
       case ACTIVATION_LOGISTIC:     { res = opennn::PerceptronLayer::ActivationFunction::Logistic;                 break;}
       case ACTIVATION_TANH:         { res = opennn::PerceptronLayer::ActivationFunction::HyperbolicTangent;        break;}
       case ACTIVATION_LINEAR:       { res = opennn::PerceptronLayer::ActivationFunction::Linear;                   break;}
       case ACTIVATION_RELU:         { res = opennn::PerceptronLayer::ActivationFunction::RectifiedLinear;          break;}
       case ACTIVATION_ELU:          { res = opennn::PerceptronLayer::ActivationFunction::ExponentialLinear;        break;}
       case ACTIVATION_SELU:         { res = opennn::PerceptronLayer::ActivationFunction::ScaledExponentialLinear;  break;}
       case ACTIVATION_SOFT_PLUS:    { res = opennn::PerceptronLayer::ActivationFunction::SoftPlus;                 break;}
       case ACTIVATION_SOFT_SIGN:    { res = opennn::PerceptronLayer::ActivationFunction::SoftSign;                 break;}
       case ACTIVATION_HARD_SIGMOID: { res = opennn::PerceptronLayer::ActivationFunction::HardSigmoid;              break;}
       }
       return res;
    }

    int NerlWorkerOpenNN::translate_loss_method_int(int loss_method)
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

    opennn::TrainingStrategy::LossMethod NerlWorkerOpenNN::translate_loss_method(int loss_method){
        opennn::TrainingStrategy::LossMethod res;
        switch (loss_method)
        {
        case LOSS_METHOD_SSE:        { res = opennn::TrainingStrategy::LossMethod::SUM_SQUARED_ERROR;        break;}
        case LOSS_METHOD_MSE:        { res = opennn::TrainingStrategy::LossMethod::MEAN_SQUARED_ERROR;       break;}
        case LOSS_METHOD_NSE:        { res = opennn::TrainingStrategy::LossMethod::NORMALIZED_SQUARED_ERROR; break;}
        case LOSS_METHOD_MINKOWSKIE: { res = opennn::TrainingStrategy::LossMethod::MINKOWSKI_ERROR;          break;}
        case LOSS_METHOD_WSE:        { res = opennn::TrainingStrategy::LossMethod::WEIGHTED_SQUARED_ERROR;   break;}
        case LOSS_METHOD_CEE:        { res = opennn::TrainingStrategy::LossMethod::CROSS_ENTROPY_ERROR;      break;}
        }
        return res;
    }
  

    int NerlWorkerOpenNN::translate_optimizer_type_int(int optimizer_type)
    {   
        int res;
        switch (optimizer_type)
        { 
        case OPTIMIZER_GD:          { res = (int)opennn::TrainingStrategy::OptimizationMethod::GRADIENT_DESCENT;              break;}
        case OPTIMIZER_SGD:         { res = (int)opennn::TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT;   break;}
        case OPTIMIZER_CGD:         { res = (int)opennn::TrainingStrategy::OptimizationMethod::CONJUGATE_GRADIENT;            break;}
        case OPTIMIZER_QUASINEUTON: { res = (int)opennn::TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD;           break;}
        case OPTIMIZER_LVM:         { res = (int)opennn::TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM; break;}
        case OPTIMIZER_ADAM:        { res = (int)opennn::TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION;    break;}
        }
        return res;
    }

    opennn::TrainingStrategy::OptimizationMethod NerlWorkerOpenNN::translate_optimizer_type(int optimizer_type){
        opennn::TrainingStrategy::OptimizationMethod res;
        switch (optimizer_type)
        { 
        case OPTIMIZER_GD:          { res = opennn::TrainingStrategy::OptimizationMethod::GRADIENT_DESCENT;              break;}
        case OPTIMIZER_SGD:         { res = opennn::TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT;   break;}
        case OPTIMIZER_CGD:         { res = opennn::TrainingStrategy::OptimizationMethod::CONJUGATE_GRADIENT;            break;}
        case OPTIMIZER_QUASINEUTON: { res = opennn::TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD;           break;}
        case OPTIMIZER_LVM:         { res = opennn::TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM; break;}
        case OPTIMIZER_ADAM:        { res = opennn::TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION;    break;}
        }
        return res;
    }

    int NerlWorkerOpenNN::translate_scaling_method_int(int scaling_method)
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

    opennn::Scaler NerlWorkerOpenNN::translate_scaling_method(int scaling_method)
    {
        opennn::Scaler res;
        switch (scaling_method)
        {
        case SCALING_NONE:    { res = opennn::Scaler::NoScaling;             break;}
        case SCALING_MINMAX:  { res = opennn::Scaler::MinimumMaximum;        break;}
        case SCALING_MEANSTD: { res = opennn::Scaler::MeanStandardDeviation; break;}
        case SCALING_STD:     { res = opennn::Scaler::StandardDeviation;     break;}
        case SCALING_LOG:     { res = opennn::Scaler::Logarithm;             break;}
        }
        return res;
    }


    int NerlWorkerOpenNN::translate_unscaling_method_int(int unscaling_method)
    {   // openNN uses the Scaler enum for both scaling and unscaling
        int res; 
        switch (unscaling_method)
        {
        case UNSCALING_NONE:    { res = (int)opennn::Scaler::NoScaling;             break;}
        case UNSCALING_MINMAX:  { res = (int)opennn::Scaler::MinimumMaximum;        break;}
        case UNSCALING_MEANSTD: { res = (int)opennn::Scaler::MeanStandardDeviation; break;}
        case UNSCALING_STD:     { res = (int)opennn::Scaler::StandardDeviation;     break;}
        case UNSCALING_LOG:     { res = (int)opennn::Scaler::Logarithm;             break;}
        }
        return res;
    }

    opennn::Scaler NerlWorkerOpenNN::translate_unscaling_method(int unscaling_method)
    {
        opennn::Scaler res;
        switch (unscaling_method)
        {
        case UNSCALING_NONE:    { res = opennn::Scaler::NoScaling;             break;}
        case UNSCALING_MINMAX:  { res = opennn::Scaler::MinimumMaximum;        break;}
        case UNSCALING_MEANSTD: { res = opennn::Scaler::MeanStandardDeviation; break;}
        case UNSCALING_STD:     { res = opennn::Scaler::StandardDeviation;     break;}
        case UNSCALING_LOG:     { res = opennn::Scaler::Logarithm;             break;}
        }
        return res;
    }
  
    int NerlWorkerOpenNN::translate_pooling_method_int(int pooling_method)
    {   
        int res; 
        switch (pooling_method)
        {
        case POOLING_NONE: { res = (int)opennn::PoolingLayer::PoolingMethod::NoPooling;      break;}
        case POOLING_MAX:  { res = (int)opennn::PoolingLayer::PoolingMethod::MaxPooling;     break;}
        case POOLING_AVG:  { res = (int)opennn::PoolingLayer::PoolingMethod::AveragePooling; break;}
        }
        return res;
    }

    opennn::PoolingLayer::PoolingMethod NerlWorkerOpenNN::translate_pooling_method(int pooling_method)
    {
        opennn::PoolingLayer::PoolingMethod res;
        switch (pooling_method)
        {
        case POOLING_NONE: { res = opennn::PoolingLayer::PoolingMethod::NoPooling;      break;}
        case POOLING_MAX:  { res = opennn::PoolingLayer::PoolingMethod::MaxPooling;     break;}
        case POOLING_AVG:  { res = opennn::PoolingLayer::PoolingMethod::AveragePooling; break;}
        }
        return res;
    }

    int NerlWorkerOpenNN::translate_model_type(int model_type, int &custom_model)
    {
        int res = model_type;
        custom_model = false;

        switch (model_type)
        {
        case MODEL_TYPE_APPROXIMATION:   {res = (int)NeuralNetwork::ProjectType::Approximation;       break;}
        case MODEL_TYPE_CLASSIFICATION:  {res = (int)NeuralNetwork::ProjectType::Classification;      break;}
        case MODEL_TYPE_FORECASTING:     {res = (int)NeuralNetwork::ProjectType::Forecasting;         break;}
        case MODEL_TYPE_NN:              {custom_model = true; break;}
        case MODEL_TYPE_AUTOENCODER:     {custom_model = true; break;} // TODO Guy consider Autoassociation type
        case MODEL_TYPE_AE_CLASSIFIER:   {custom_model = true; break;}
        }
        return res;
    }

} // namespace nerlnet