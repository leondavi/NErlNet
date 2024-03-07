#include "nerlWorkerOpenNN.h"

using namespace opennn;

namespace nerlnet
{
// ----- NerlWorkerOpenNN -----

    NerlWorkerOpenNN::NerlWorkerOpenNN(int model_type,std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                     float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                     int loss_method, int distributed_system_type, std::string &distributed_system_args_str) : NerlWorker(model_type, layer_sizes_str, layer_types_list, layers_functionality,
                                                                                                                      learning_rate, epochs, optimizer_type, optimizer_args_str,
                                                                                                                      loss_method, distributed_system_type, distributed_system_args_str)
    {
        _neural_network_ptr = std::make_shared<opennn::NeuralNetwork>();
        generate_opennn_neural_network();
        _training_strategy_ptr = std::make_shared<opennn::TrainingStrategy>();
        generate_training_strategy();
    }

    NerlWorkerOpenNN::~NerlWorkerOpenNN()
    {

    }

    void NerlWorkerOpenNN::post_training_process()
    {
        switch(_model_type){
            case MODEL_TYPE_NN:
            {
                break;
            }
            case MODEL_TYPE_AUTOENCODER:
            {
                break; // Get Loss Values by class LossIndexBackPropagationLM
            }
            case MODEL_TYPE_AE_CLASSIFIER:
            {
                break; // Get Loss Values , Add RED support - David's Thesis
            }
            // case MODEL_TYPE_LSTM:
            // {
            //     break;
            // }
            // case MODEL_TYPE_RECURRENT:
            // {
            //     break;
            // }
        } 
    }

    void NerlWorkerOpenNN::post_predict_process(fTensor2DPtr result_ptr){
        switch(_model_type){
            case MODEL_TYPE_NN:
            {
                break;
            }
            case MODEL_TYPE_AUTOENCODER:
            {
                break;
            }
            case MODEL_TYPE_AE_CLASSIFIER:
            {
                break; // Calculate MSE between result and input , then apply AE_RED
            }
            // case MODEL_TYPE_LSTM:
            // {
            //     break;
            // }
            // case MODEL_TYPE_RECURRENT:
            // {
            //     break;
            // }
        }
    
    }

    /**
     * @brief generate the training strategy for the opennn neural network
     * this function doesn't set the data pointer of the training strategy and doesn't call to the train function
    **/
    void NerlWorkerOpenNN::generate_training_strategy()
    {
     _training_strategy_ptr->set_neural_network_pointer(_neural_network_ptr.get()); // Neural network must be defined at this point
     set_optimization_method(_optimizer_type,_learning_rate);
     set_loss_method(_loss_method);
     _training_strategy_ptr->set_maximum_epochs_number(_epochs); 
     //_training_strategy_ptr->set_display(TRAINING_STRATEGY_SET_DISPLAY_OFF); // remove opennn training strategy prints
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
                    case MODEL_TYPE_AUTOENCODER: // ! Ask David if AE/AEC should be created in "generate_custom_model_nn" (same building process)
                    {
                        generate_custom_model_nn(_neural_network_ptr); // TODO Change back to generate_custom_model_ae
                        break;
                    }
                    case MODEL_TYPE_AE_CLASSIFIER: // ! Ask David if AE/AEC should be created in "generate_custom_model_nn" (same building process)
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

    void NerlWorkerOpenNN::set_dataset(std::shared_ptr<opennn::DataSet> data_set,fTensor2DPtr TrainDataNNptr){
        _data_set = data_set;
        int nerlnet_custom_model; // defines if this is a nerlnet custom project or an opennn project
        /// find the type of input layer (first layer) and neural network 
        // if needed take the data set and change it (in cnn change,in ae duplicate)
        // in cnn , see the inputs of the user. if the user gave 3 dimensions so the data set will have 3 dimensions
        // the last dim will be the samples num and the second will be multiple channels and  columns .
        std::shared_ptr<opennn::NeuralNetwork> neural_network_ptr = get_neural_network_ptr();
         switch (_model_type) {
            case MODEL_TYPE_AUTOENCODER:
            {

            }
            case MODEL_TYPE_AE_CLASSIFIER:
            {
            cout << TrainDataNNptr->dimension(0) << "x" << TrainDataNNptr->dimension(1) << endl;
            Eigen::array<int, 2> bcast({1, 2}); 
            std::shared_ptr<Eigen::Tensor<float,2>> autoencoder_data = std::make_shared<Eigen::Tensor<float,2>>(TrainDataNNptr->broadcast(bcast));
            int num_of_features = neural_network_ptr->get_inputs_number();
            int num_of_output_neurons = neural_network_ptr->get_outputs_number(); 
            bool data_set_condition = (num_of_features + num_of_output_neurons) == autoencoder_data->dimension(1);
            assert(("issue with data input/output dimensions", data_set_condition));
            _data_set->set_data(*autoencoder_data);
            _data_set->set(autoencoder_data->dimension(0) , num_of_features , num_of_output_neurons); // TODO CHECK

            _data_set->print();
            neural_network_ptr->print();
            break;
            }
            default:
            {
            int data_cols = TrainDataNNptr->dimension(1);
            int num_of_features = neural_network_ptr->get_inputs_number();
            int num_of_output_neurons = neural_network_ptr->get_outputs_number(); 

             // Data set definitions
             bool data_set_condition = (num_of_features + num_of_output_neurons) == data_cols;
             assert(("issue with data input/output dimensions", data_set_condition));
            _data_set->set_data(*(TrainDataNNptr));
            _data_set->set(TrainDataNNptr->dimension(0), num_of_features, num_of_output_neurons);
            }
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
                   // std::shared_ptr<NerlLayer> prev_layer = curr_layer->get_prev_layer_ptr();
                    int layer_rows_num     = curr_layer->get_dim_size(DIM_X_IDX);
                    int layer_cols_num     = curr_layer->get_dim_size(DIM_Y_IDX);
                    int layer_channels_num = curr_layer->get_dim_size(DIM_Z_IDX);
                    std::shared_ptr<NerlLayerCNN> cnn_curr_layer = std::dynamic_pointer_cast<NerlLayerCNN>(curr_layer);
                    // set the number of inputs
                    Tensor<Index, 1> cnn_layer_inputs_dimensions(4); 
                    cnn_layer_inputs_dimensions[0] = layer_rows_num;
                    cnn_layer_inputs_dimensions[1] = layer_cols_num;
                    cnn_layer_inputs_dimensions[2] = layer_channels_num; // each channel is diffrent value in the matrix (in rgb it is 3)
                    cnn_layer_inputs_dimensions[3] = 1; //number of images/data 
                    int kernels_rows_number    = cnn_curr_layer->get_dim_kernel_size(DIM_X_IDX);
                    int kernels_columns_number = cnn_curr_layer->get_dim_kernel_size(DIM_Y_IDX);
                    int kernels_number = 1; 
                    // set the number of kernel
                    Tensor<Index, 1> cnn_layer_kernels_dimensions(4);
                    cnn_layer_kernels_dimensions(0) = kernels_rows_number; //according the opennn example
                    cnn_layer_kernels_dimensions(1) = kernels_columns_number; //according the opennn example
                    cnn_layer_kernels_dimensions(2) = layer_channels_num; //change to get_dim_kernel_size z
                    cnn_layer_kernels_dimensions(3) = kernels_number; //according the opennn example
                    ConvolutionalLayer* convolutional_layer = new ConvolutionalLayer(cnn_layer_inputs_dimensions, cnn_layer_kernels_dimensions);
                    //set stride
                    int stride_row = cnn_curr_layer->get_stride(DIM_X_IDX);
                    int stride_col = cnn_curr_layer->get_stride(DIM_Y_IDX);
                    convolutional_layer->set_row_stride(stride_row); // set_row_stride
                    convolutional_layer->set_column_stride(stride_col); // set_column_stride
                    //set padding add if to make sure we have padding
                 //   Tensor<type,4> cnn_layer_padding_dimensions(2);
                   // Tensor<type,4> cnn_layer_padding_outputs(layer_rows_num, layer_cols_num, layer_channels_num, 1);
                    int padding_row =  cnn_curr_layer->get_padding_size(DIM_X_IDX);
                    int padding_col =  cnn_curr_layer->get_padding_size(DIM_Y_IDX);
                    //cnn_layer_padding_dimensions(0) =  padding_row; //according the opennn example
                   // cnn_layer_padding_dimensions(1) =  padding_col; //according the opennn example
                    // set convulution type
                    if(padding_row == 0 && padding_col == 0){
                        convolutional_layer->set_convolution_type(opennn::ConvolutionalLayer::ConvolutionType::Valid); 
                    }else{
                        convolutional_layer->set_convolution_type(opennn::ConvolutionalLayer::ConvolutionType::Same); 
                    }
                    // insert padding
                    //convolutional_layer->insert_padding(cnn_layer_padding_dimensions,cnn_layer_padding_outputs);
                    // set activation function
                    convolutional_layer->set_activation_function((opennn::ConvolutionalLayer::ActivationFunction)(cnn_curr_layer->get_layer_functionality())); // set activation function
                    // add layer to the neural network
                    neural_network_ptr->add_layer(convolutional_layer); // add layer to the neural network
                    if(curr_layer->get_next_layer_ptr()->get_layer_type() == LAYER_TYPE_PERCEPTRON){ // if the next layer is perceptron
                            Tensor<Index, 1> flatten_layer_inputs_dimensions(4);
                            flatten_layer_inputs_dimensions(0) =  ((layer_rows_num - kernels_rows_number    + 2*padding_row*0) / stride_row) + 1; //according the opennn example
                            flatten_layer_inputs_dimensions(1) =  ((layer_cols_num - kernels_columns_number + 2*padding_col*0) / stride_col) + 1; //according the opennn example
                            flatten_layer_inputs_dimensions(2) =  kernels_number; //according the opennn example
                            flatten_layer_inputs_dimensions(3) =  1;             //samples_number
                            FlattenLayer* flatten_layer = new FlattenLayer(flatten_layer_inputs_dimensions); // create flatten layer
                            // TODO :Talk with Noa and Ohad about the sizes - make sure the sizes are correct in NerlPlanner
                            std::cout << "flatten_layer->get_outputs_dimensions()[0] = " << flatten_layer->get_outputs_dimensions()[0] << std::endl;
                            if (flatten_layer->get_outputs_dimensions()[0] != curr_layer->get_next_layer_ptr()->get_dim_size(DIM_X_IDX)) // make sure the dims correct
                            {
                               LogError("NerlWorkerOpenNN::generate_custom_model_nn - wrong dimensions in CNN and Perceptron");
                               throw std::invalid_argument("NerlWorkerOpenNN::generate_custom_model_nn - wrong dimensions in CNN  and Perceptron");
                            }
                            neural_network_ptr->add_layer(flatten_layer);
                    }
                    break;

                }
                 case LAYER_TYPE_POOLING:
                {
                   // layer type is pooling
                   int layer_rows_num     = curr_layer->get_dim_size(DIM_X_IDX);
                   int layer_cols_num     = curr_layer->get_dim_size(DIM_Y_IDX);
                   //dynamic cast to NerlLayerPooling
                   std::shared_ptr<NerlLayerPooling> pooling_curr_layer = std::dynamic_pointer_cast<NerlLayerPooling>(curr_layer);
                   //get pooling dims
                   std::vector<int> pooling_dims;
                   pooling_curr_layer->get_pooling_dims(pooling_dims);
                   //create pooling layer
                   PoolingLayer* pooling_layer = new PoolingLayer(layer_rows_num, layer_cols_num);
                   // set pooling layer parameters
                   pooling_layer->set_pool_size(pooling_dims[0],pooling_dims[1]);
                   pooling_layer->set_row_stride(pooling_curr_layer->get_stride(DIM_X_IDX));
                   pooling_layer->set_column_stride(pooling_curr_layer->get_stride(DIM_Y_IDX));
                   pooling_layer->set_pooling_method(translate_pooling_method(pooling_curr_layer->get_layer_functionality()));
                   pooling_layer->set_padding_width(pooling_curr_layer->get_padding_size(DIM_X_IDX));
                   neural_network_ptr->add_layer(pooling_layer); // add layer to the neural network
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
                case LAYER_TYPE_SCALING: // TODO Check this layer implementation
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
                  case LAYER_TYPE_UNSCALING: // TODO Check this layer implementation
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

                // TODO Add Boudning Layer!

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
                    newLayer->set_activation_function(translate_probabilistic_activation_function(get_layer_functionality));
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

 ProbabilisticLayer::ActivationFunction NerlWorkerOpenNN::translate_probabilistic_activation_function(int activation_function)
    {
    ProbabilisticLayer::ActivationFunction res;
        switch (activation_function)
         {
          case PROBABILISTIC_ACTIVATION_BINARY:      { res = opennn::ProbabilisticLayer::ActivationFunction::Binary;                break;}  
          case PROBABILISTIC_ACTIVATION_LOGISTIC:    { res = opennn::ProbabilisticLayer::ActivationFunction::Logistic;              break;}
          case PROBABILISTIC_ACTIVATION_COMPETITIVE: { res = opennn::ProbabilisticLayer::ActivationFunction::Competitive;           break;}
          case PROBABILISTIC_ACTIVATION_SOFTMAX:     { res = opennn::ProbabilisticLayer::ActivationFunction::Softmax;               break;}
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
        case MODEL_TYPE_AUTOENCODER:     {custom_model = true; break;}
        case MODEL_TYPE_AE_CLASSIFIER:   {custom_model = true; break;}
        }
        return res;
    }

} // namespace nerlnet