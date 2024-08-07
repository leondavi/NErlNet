#include "nerlWorkerOpenNN.h"
#include "ae_red.h" 

using namespace opennn;

namespace nerlnet
{
// ----- NerlWorkerOpenNN -----

    NerlWorkerOpenNN::NerlWorkerOpenNN(int model_type, std::string &model_args_str , std::string &layer_sizes_str, std::string &layer_types_list, std::string &layers_functionality,
                    float learning_rate, int epochs, int optimizer_type, std::string &optimizer_args_str,
                    int loss_method, std::string &loss_args_str, int distributed_system_type, std::string &distributed_system_args_str) : NerlWorker(model_type, model_args_str , layer_sizes_str, layer_types_list, layers_functionality,
                                                                                                                    learning_rate, epochs, optimizer_type, optimizer_args_str,
                                                                                                                    loss_method, loss_args_str, distributed_system_type, distributed_system_args_str)
    {
        _neural_network_ptr = std::make_shared<opennn::NeuralNetwork>();
        generate_opennn_neural_network();
        _training_strategy_ptr = std::make_shared<opennn::TrainingStrategy>();
        generate_training_strategy();
        _ae_red_ptr = std::make_shared<AeRed>();
    }

    NerlWorkerOpenNN::~NerlWorkerOpenNN()
    {

    }

    void NerlWorkerOpenNN::perform_training()
    {
        this->_training_strategy_ptr->set_data_set_pointer(this->_data_set.get());

        TrainingResults res = this->_training_strategy_ptr->perform_training();
        this->_last_loss = res.get_training_error();
    
        switch (_model_type)
        {
            default:
            {
                break;
            }
        }
    }

    fTensor2DPtr NerlWorkerOpenNN::get_loss_nerltensor()
    {
        fTensor2DPtr loss_val_tensor;

        switch (_model_type)
        {
            case MODEL_TYPE_AE_CLASSIFIER:
            {
                loss_val_tensor = std::make_shared<fTensor2D>(3, 1);
                (*loss_val_tensor)(0, 0) = static_cast<float>(_last_loss);
                (*loss_val_tensor)(1, 0) = _ae_red_ptr->_ema_event;
                (*loss_val_tensor)(2, 0) = _ae_red_ptr->_ema_normal;
                break;
            }
            default:
            {
                 loss_val_tensor = std::make_shared<fTensor2D>(1, 1); // allocate tensor for loss value
                (*loss_val_tensor)(0, 0) = static_cast<float>(_last_loss); // set loss value to tensor
            }
        }

        return loss_val_tensor;

    } 

    void NerlWorkerOpenNN::post_training_process(fTensor2DPtr TrainData)
    {
        switch(_model_type){
            case MODEL_TYPE_NN:
            {
                break;
            }
            case MODEL_TYPE_AUTOENCODER: // Get Loss Values by class LossIndexBackPropagationLM
            {
                // std::shared_ptr<TrainingStrategy> training_strategy_ptr = get_training_strategy_ptr();
                // OptimizationAlgorithm* optimizer = training_strategy_ptr->get_optimization_algorithm_pointer();
                // LossIndex* loss_index = optimizer->get_loss_index_pointer();
                // std::shared_ptr<NeuralNetwork> _neural_network_pointer = get_neural_network_ptr();
                // Tensor<Layer*, 1> trainable_layers_pointers = _neural_network_pointer->get_trainable_layers_pointers();
                // cout << "Gradient: " << trainable_layers_pointers[0] << endl;
                // cout << "LossType: " << loss_index->get_error_type() << endl;

            }
            case MODEL_TYPE_AE_CLASSIFIER: // Get Loss Values , Add RED support - David's Thesis
            {
                std::shared_ptr<opennn::NeuralNetwork> neural_network = get_neural_network_ptr();
                Index num_of_samples = _aec_data_set->dimension(0);
                Index num_of_labels = 1;
                Index inputs_number = neural_network->get_inputs_number();
                Tensor<Index, 1> inputs_dimensions(2);
                inputs_dimensions.setValues({num_of_samples, inputs_number});
                fTensor2DPtr results = std::make_shared<fTensor2D>(num_of_samples, num_of_labels + num_of_labels*2); // LOWER/UPPER for each label
                fTensor2DPtr calculate_res = std::make_shared<fTensor2D>(num_of_samples, neural_network->get_outputs_number());
                *calculate_res = neural_network->calculate_outputs(TrainData->data(), inputs_dimensions);

                // MSE Calculation
                fTensor2DPtr loss_values_mse = std::make_shared<fTensor2D>(num_of_samples, 1);
                fTensor2D diff = (*calculate_res - *_aec_data_set);
                fTensor2D squared_diff = diff.pow(2);
                fTensor1D sum_squared_diff = squared_diff.sum(Eigen::array<int, 1>({1}));
                fTensor1D mse1D = (1.0 / static_cast<float>(_aec_data_set->dimension(0))) * sum_squared_diff;
                fTensor2D mse2D = mse1D.reshape(Eigen::array<int, 2>({num_of_samples, 1}));
                *loss_values_mse = mse2D;

                _ae_red_ptr->update_batch(loss_values_mse); // Update thresholds
                
                break;
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
    

    void NerlWorkerOpenNN::post_predict_process(fTensor2DPtr &result_ptr){
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
                std::shared_ptr<opennn::NeuralNetwork> neural_network = get_neural_network_ptr();
                Index num_of_samples = _aec_data_set->dimension(0);
                Index inputs_number = neural_network->get_inputs_number();
                Index num_of_labels = 1;
                fTensor2DPtr results = std::make_shared<fTensor2D>(num_of_samples, num_of_labels); 
                fTensor2DPtr loss_values_mse = std::make_shared<fTensor2D>(num_of_samples , 1);
                fTensor2D diff = (*result_ptr - *_aec_data_set);
                fTensor2D squared_diff = diff.pow(2);
                fTensor1D sum_squared_diff = squared_diff.sum(Eigen::array<int, 1>({1}));
                fTensor1D mse1D = (1.0 / static_cast<float>(_aec_data_set->dimension(0))) * sum_squared_diff;
                fTensor2D mse2D = mse1D.reshape(Eigen::array<int, 2>({num_of_samples, 1}));
                *loss_values_mse = mse2D;
                result_ptr = _ae_red_ptr->update_batch(loss_values_mse);
                break;
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
      // TODO Ori add here the parsing of loss args
      // _training_strategy_ptr->get_loss_index_pointer()->set_regularization_method(reg_val);
      _training_strategy_ptr->set_maximum_epochs_number(_epochs); 
      _training_strategy_ptr->set_display(TRAINING_STRATEGY_SET_DISPLAY_OFF); 
    }

    void NerlWorkerOpenNN::set_optimization_method(int optimizer_type,int learning_rate){
        assert((_training_strategy_ptr->has_neural_network(), "NerlWorkerOpenNN::set_optimization_method - neural network pointer is null"));
        _optimizer_type = optimizer_type;
        //cout << "optimizer_type = " << optimizer_type << endl;
        _training_strategy_ptr->set_optimization_method(translate_optimizer_type(optimizer_type));
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
                        generate_custom_model_nn(_neural_network_ptr); 
                        break;
                    }
                    case MODEL_TYPE_AE_CLASSIFIER: // ! Ask David if AE/AEC should be created in "generate_custom_model_nn" (same building process)
                    {
                        generate_custom_model_nn(_neural_network_ptr);
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

 
    void NerlWorkerOpenNN::get_result_calc(fTensor2DPtr calculate_res,int num_of_samples,int inputs_number,fTensor2DPtr predictData){
        Tensor<Index, 1> input_variable_dimension(4);
        Tensor<Index, 1> inputs_dimensions(2);
        std::shared_ptr<opennn::NeuralNetwork> neural_network_ptr = get_neural_network_ptr();
         if(neural_network_ptr->has_convolutional_layer())
    {  
        ConvolutionalLayer* conv = (ConvolutionalLayer*)neural_network_ptr->get_layer_pointer(0);
        input_variable_dimension.setValues({num_of_samples,conv->get_input_variables_dimensions()(1), conv->get_input_variables_dimensions()(2), conv->get_input_variables_dimensions()(3)});
        *calculate_res = neural_network_ptr->calculate_outputs(predictData->data(), input_variable_dimension);
    }else{
        inputs_dimensions.setValues({num_of_samples, inputs_number});
         *calculate_res = neural_network_ptr->calculate_outputs(predictData->data(), inputs_dimensions);
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
                _aec_data_set = TrainDataNNptr; 
                Eigen::array<int, 2> bcast({1, 2}); 
                std::shared_ptr<Eigen::Tensor<float,2>> autoencoder_data = std::make_shared<Eigen::Tensor<float,2>>(TrainDataNNptr->broadcast(bcast));
                int num_of_features = neural_network_ptr->get_inputs_number();
                int num_of_output_neurons = neural_network_ptr->get_outputs_number();
                bool data_set_condition = (num_of_features + num_of_output_neurons) == autoencoder_data->dimension(1);
                assert(("issue with data input/output dimensions", data_set_condition));
                _data_set->set_data(*autoencoder_data);
                _data_set->set(autoencoder_data->dimension(0) , num_of_features , num_of_output_neurons); // TODO CHECK
                break;
            }
            default:
            {  
                int data_cols = TrainDataNNptr->dimension(1);
                int num_of_features = neural_network_ptr->get_inputs_number();
                int num_of_output_neurons = neural_network_ptr->get_outputs_number(); 
                _data_set->set_data(*(TrainDataNNptr));
                // Data set definitions
                bool data_set_condition = (num_of_features + num_of_output_neurons) == data_cols;
                assert(("issue with data input/output dimensions", data_set_condition));
                if(neural_network_ptr->has_convolutional_layer()){
                    Tensor<Index, 1> input_variable_dimension(3);
                    input_variable_dimension.setValues({this->_nerl_layers_linked_list->get_dim_size(DIM_Z_IDX), this->_nerl_layers_linked_list->get_dim_size(DIM_Y_IDX), this->_nerl_layers_linked_list->get_dim_size(DIM_X_IDX)});
                    _data_set->set_input_variables_dimensions(input_variable_dimension);
                    int samples_num =  _data_set->get_samples_number();
                    int input_variable = this->_nerl_layers_linked_list->get_dim_size(DIM_Y_IDX)* this->_nerl_layers_linked_list->get_dim_size(DIM_X_IDX);
                    for(Index sample_indx = 0; sample_indx < samples_num ; sample_indx++)
                    {
                    _data_set->set_sample_use(sample_indx, DataSet::SampleUse::Training);
                    }
                
                    for(Index column_indx = 0; column_indx <input_variable ; column_indx++)
                    {
                    _data_set->set_column_use(column_indx, DataSet::VariableUse::Input);
                    _data_set->set_column_type(column_indx, DataSet::ColumnType::Numeric);
                    }
                    for(Index column_indx = input_variable; column_indx < input_variable + num_of_output_neurons; column_indx++)
                    {
                        _data_set->set_column_type(column_indx, DataSet::ColumnType::Binary);
                        _data_set->set_column_use(column_indx, DataSet::VariableUse::Target);
                    }
                        _data_set->set_columns_scalers(Scaler::NoScaling);
                }else{
                        _data_set->set(TrainDataNNptr->dimension(0), num_of_features, num_of_output_neurons);
                }
                break;
            }
         }
         //------------ Distributed System Type ------------
         switch (_distributed_system_type)
         {
            case WORKER_DISTRIBUTED_SYSTEM_TYPE_FEDCLIENTWEIGHTEDAVGCLASSIFICATION: // Federated Client Weighted Average Classification
            {
                int col_num = _data_set->get_columns_number();
                int num_of_output_neurons = _neural_network_ptr->get_outputs_number(); 
                Tensor<Index, 1> selected_column_indices(num_of_output_neurons);
                // selected_column_indices is the indices of the labels, it's wrote
                // like that because there is a meaning to the order of the labels
                // it's used in get_columns_data to get the labels (last columns in the data set)
                for(int i =0;i<num_of_output_neurons;i++){
                    selected_column_indices(i) = col_num - num_of_output_neurons + i;
                }
                Tensor<type, 2> labels = _data_set->get_columns_data(selected_column_indices);
                Tensor<type, 1> rowSum = labels.sum(Eigen::array<int, 1>{0}); // sum of the rows - each col is labels , each row is a sample
                std::vector<int> rowSumVec;
                size_t tensorSize = rowSum.size();
                float* tensorData = rowSum.data();
                for (size_t i = 0; i < tensorSize; ++i) {
                    rowSumVec.push_back(tensorData[i]); // copy the data to the vector from tensor
                }
                _train_labels_count = std::make_shared<std::vector<int>>(rowSumVec);
                break;
            }
            default:
            {
                break;
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
                case LAYER_TYPE_CONV:
                {
                    int layer_rows_num     = curr_layer->get_dim_size(DIM_X_IDX);
                    int layer_cols_num     = curr_layer->get_dim_size(DIM_Y_IDX);
                    int layer_channels_num = curr_layer->get_dim_size(DIM_Z_IDX);
                    std::shared_ptr<NerlLayerCNN> cnn_curr_layer = std::dynamic_pointer_cast<NerlLayerCNN>(curr_layer);
                    // set the number of inputs
                    Tensor<Index, 1> cnn_layer_inputs_dimensions(4); 
                    cnn_layer_inputs_dimensions[Convolutional4dDimensions::sample_index] = 1;
                    cnn_layer_inputs_dimensions[Convolutional4dDimensions::row_index] = layer_rows_num;
                    cnn_layer_inputs_dimensions[Convolutional4dDimensions::column_index] = layer_cols_num;
                    cnn_layer_inputs_dimensions[Convolutional4dDimensions::channel_index] = layer_channels_num;
                    int kernels_rows_number     = cnn_curr_layer->get_dim_kernel_size(DIM_X_IDX);
                    int kernels_columns_number  = cnn_curr_layer->get_dim_kernel_size(DIM_Y_IDX);
                    int kernels_number          = cnn_curr_layer->get_dim_kernel_size(DIM_W_IDX); 
                    int kernels_channels_number = cnn_curr_layer->get_dim_kernel_size(DIM_Z_IDX); 

                    // set the number of kernel
                    Tensor<Index, 1> cnn_layer_kernels_dimensions(4);
                    cnn_layer_kernels_dimensions[Kernel4dDimensions::row_index]     = kernels_rows_number; //according the opennn example
                    cnn_layer_kernels_dimensions[Kernel4dDimensions::column_index]  = kernels_columns_number; //according the opennn example
                    cnn_layer_kernels_dimensions[Kernel4dDimensions::channel_index] = kernels_channels_number; //change to get_dim_kernel_size z
                    cnn_layer_kernels_dimensions[Kernel4dDimensions::kernel_index]  = kernels_number;    //according the opennn example
                
                    ConvolutionalLayer* convolutional_layer = new ConvolutionalLayer(cnn_layer_inputs_dimensions, cnn_layer_kernels_dimensions);
                    //set stride
                    int stride_row  = cnn_curr_layer->get_stride(DIM_X_IDX);
                    int stride_col  = cnn_curr_layer->get_stride(DIM_Y_IDX);
    
                    convolutional_layer->set_column_stride(stride_col); // set_column_stride
                    convolutional_layer->set_row_stride(stride_row); // set_row_stride

                    //set padding add if to make sure we have padding
                 //   Tensor<type,4> cnn_layer_padding_dimensions(2);
                   // Tensor<type,4> cnn_layer_padding_outputs(layer_rows_num, layer_cols_num, layer_channels_num, 1);
                    int padding_row =  cnn_curr_layer->get_padding_size(DIM_X_IDX);
                    int padding_col =  cnn_curr_layer->get_padding_size(DIM_Y_IDX);
                    //cnn_layer_padding_dimensions(0) =  padding_row; //according the opennn example
                   // cnn_layer_padding_dimensions(1) =  padding_col; //according the opennn example
                    // set convulution type
                    if(cnn_curr_layer->get_type_conv()){
                         convolutional_layer->set_convolution_type(opennn::ConvolutionalLayer::ConvolutionType::Same);
                    }else{
                        convolutional_layer->set_convolution_type(opennn::ConvolutionalLayer::ConvolutionType::Valid); 
                    }
                    // insert padding
                    //convolutional_layer->insert_padding(cnn_layer_padding_dimensions,cnn_layer_padding_outputs);
                    // set activation function
                    convolutional_layer->set_activation_function((opennn::ConvolutionalLayer::ActivationFunction)(cnn_curr_layer->get_layer_functionality())); // set activation function
                    // add layer to the neural network
                    neural_network_ptr->add_layer(convolutional_layer); // add layer to the neural network
                    break;

                }
                 case LAYER_TYPE_POOLING:
                {
                   // layer type is pooling
                   int layer_rows_num     = curr_layer->get_dim_size(DIM_X_IDX);
                   int layer_cols_num     = curr_layer->get_dim_size(DIM_Y_IDX);
                   int layer_channels_num = curr_layer->get_dim_size(DIM_Z_IDX);
                   //dynamic cast to NerlLayerPooling
                   std::shared_ptr<NerlLayerPooling> pooling_curr_layer = std::dynamic_pointer_cast<NerlLayerPooling>(curr_layer);
                   //get pooling dims
                   int pooling_row = pooling_curr_layer->get_dim_pooling_size(DIM_X_IDX);
                   int pooling_col = pooling_curr_layer->get_dim_pooling_size(DIM_Y_IDX);
                    Tensor<Index, 1> pooling_dimension(2);
                    pooling_dimension.setValues({
                       pooling_row,
                       pooling_col
                      });
                   //create pooling layer
                    Tensor<Index, 1> input_dimensions(4);
                    input_dimensions(Convolutional4dDimensions::channel_index) = layer_channels_num; // Number of kernels (Channels)
                    input_dimensions(Convolutional4dDimensions::row_index)     = layer_rows_num; // Rows
                    input_dimensions(Convolutional4dDimensions::column_index)  = layer_cols_num; // Cols
            
                   PoolingLayer* pooling_layer = new PoolingLayer(input_dimensions,pooling_dimension);
                   // set pooling layer parameters
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
                    LogError("NerlWorkerOpenNN::generate_custom_model_nn - wrong layer type");
                    throw std::invalid_argument("NerlWorkerOpenNN::generate_custom_model_nn - wrong layer type");
                    break;
                }
                case LAYER_TYPE_PERCEPTRON:
                {
                    if (curr_layer->is_first())
                    {
                       LogError("NerlWorkerOpenNN::generate_custom_model_nn - PERCEPTRON cannot be first layer");
                       throw std::invalid_argument("NerlWorkerOpenNN::generate_custom_model_nn - PERCEPTRON cannot be first layer");
                    }
                    int prev_layer_size;
                    if(neural_network_ptr->get_layer_pointer(neural_network_ptr->get_layers_number()-1)->get_type_string() == "Flatten"){
                           prev_layer_size = ((FlattenLayer*)(neural_network_ptr->get_layer_pointer(neural_network_ptr->get_layers_number()-1)))->get_outputs_dimensions()[1];
                    }else{
                          std::shared_ptr<NerlLayer> prev_layer = curr_layer->get_prev_layer_ptr();
                          prev_layer_size = prev_layer->get_dim_size(DIM_X_IDX);
                    }
                    int layer_size_curr = curr_layer->get_dim_size(DIM_X_IDX);
                    int get_layer_functionality = curr_layer->get_layer_functionality();
                    PerceptronLayer* newLayer =  new opennn::PerceptronLayer(prev_layer_size, layer_size_curr);
                    newLayer->set_activation_function(translate_activation_function(get_layer_functionality));
                    neural_network_ptr->add_layer(newLayer);
                    break;
                }
                case LAYER_TYPE_SCALING: // TODO Check this layer implementation
                {
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

                case LAYER_TYPE_BOUNDING: // TODO Check this layer implementation
                {
                    std::vector<int> layer_dims_vec;
                    curr_layer->get_layer_size(layer_dims_vec);
                    int layer_size_curr = curr_layer->get_dim_size(DIM_X_IDX);
                    int get_layer_functionality = curr_layer->get_layer_functionality();
                    BoundingLayer* newLayer = new opennn::BoundingLayer(layer_size_curr);
                    newLayer->set_bounding_method("Bounding"); // ! What this method should be?
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
                    if(layer_size_curr<=1){
                        LogError("NerlWorkerOpenNN::generate_custom_model_nn - PROBABILISTIC cannot be smaller or equal to 1");
                        throw std::invalid_argument("NerlWorkerOpenNN::generate_custom_model_nn -PROBABILISTIC cannot be smaller or equal to 1");
                    }            
                    int get_layer_functionality = curr_layer->get_layer_functionality();
                    ProbabilisticLayer* newLayer =  new opennn::ProbabilisticLayer(prev_layer_size, layer_size_curr);
                    newLayer->set_activation_function(translate_probabilistic_activation_function(get_layer_functionality));
                    neural_network_ptr->add_layer(newLayer);
                    break;
                }
                case LAYER_TYPE_FLATTEN:
                {
                        int layer_rows_num     = curr_layer->get_dim_size(DIM_X_IDX);
                        int layer_cols_num     = curr_layer->get_dim_size(DIM_Y_IDX);
                        int layer_channels_num = curr_layer->get_dim_size(DIM_Z_IDX);
                        if(curr_layer->get_prev_layer_ptr()->get_layer_type() == LAYER_TYPE_CONV) { // if the next layer is perceptron       
                            ConvolutionalLayer*  cnn_prev_layer = (ConvolutionalLayer*)neural_network_ptr->get_layer_pointer((neural_network_ptr->get_layers_number()-1));
                            FlattenLayer* flatten_layer = new FlattenLayer(cnn_prev_layer->get_outputs_dimensions()); // create flatten layer
                            neural_network_ptr->add_layer(flatten_layer);
                        } else {
                            Tensor<Index, 1> input_variables_dimensions(3);
                            input_variables_dimensions(Convolutional4dDimensions::channel_index) = layer_channels_num;
                            input_variables_dimensions(Convolutional4dDimensions::column_index)  = layer_cols_num;
                            input_variables_dimensions(Convolutional4dDimensions::row_index)     = layer_rows_num;
                            FlattenLayer* flatten_layer = new FlattenLayer(input_variables_dimensions); // create flatten layer
                            neural_network_ptr->add_layer(flatten_layer);
                          }
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
            case LAYER_TYPE_CONV:          { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_PERCEPTRON:   { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_POOLING:      { res = translate_pooling_method_int(layer_functionality);      break;}
            case LAYER_TYPE_PROBABILISTIC:{ res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_LSTM:         { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_RECCURRENT:   { res = translate_activation_function_int(layer_functionality); break;}
            case LAYER_TYPE_UNSCALING:    { res = translate_unscaling_method_int(layer_functionality);    break;}
            case LAYER_TYPE_BOUNDING:     { res = translate_activation_function_int(layer_functionality); break;} // TODO Ori this is an error - bounding error should not have activation function
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
            case LAYER_TYPE_CONV:          { res = (int)opennn::Layer::Type::Convolutional;       break;}
            case LAYER_TYPE_PERCEPTRON:   { res = (int)opennn::Layer::Type::Perceptron;          break;}
            case LAYER_TYPE_POOLING:      { res = (int)opennn::Layer::Type::Pooling;             break;}
            case LAYER_TYPE_PROBABILISTIC:{ res = (int)opennn::Layer::Type::Probabilistic;       break;}
            case LAYER_TYPE_LSTM:         { res = (int)opennn::Layer::Type::LongShortTermMemory; break;}
            case LAYER_TYPE_RECCURRENT:   { res = (int)opennn::Layer::Type::Recurrent;           break;}
            case LAYER_TYPE_UNSCALING:    { res = (int)opennn::Layer::Type::Unscaling;           break;}
            case LAYER_TYPE_FLATTEN:      { res = (int)opennn::Layer::Type::Flatten;             break;}
            case LAYER_TYPE_BOUNDING:     { res = (int)opennn::Layer::Type::Bounding;            break;}
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

    std::shared_ptr<std::vector<int>> NerlWorkerOpenNN::get_distributed_system_train_labels_count()
    {       
         switch (_distributed_system_type)
        {
            case WORKER_DISTRIBUTED_SYSTEM_TYPE_FEDCLIENTWEIGHTEDAVGCLASSIFICATION: // Federated Client Weighted Average Classification
            {
                 if (_data_set == nullptr)
                    {
                       LogError("NerlWorkerOpenNN::generate_custom_model_nn - _data_set is nullptr");
                       throw std::invalid_argument("NerlWorkerOpenNN::generate_custom_model_nn - _data_set is nullptr");
                    }
                return _train_labels_count;
                break;
            }
            default:
            {
                break;
            }
        }
    }
    
    
    opennn::LossIndex::RegularizationMethod NerlWorkerOpenNN::parse_loss_args(const std::string &loss_args)
    {
        enum LossArgsEnum{L1=0,L2=1,NONE=2};
        string L1_text = "L1";
        string L2_text = "L2";
        std::string delimiter = "reg=";
        std::string token = loss_args.substr(loss_args.find(delimiter)+delimiter.length());
        std::cout << "token: " << token << std::endl;
        int loss_arg = token == L1_text ? L1 : (token == L2_text ? L2 : NONE);
        std::cout << "loss_arg: " << loss_arg << std::endl;
        switch (loss_arg)
        {
        case L1:
            return opennn::LossIndex::RegularizationMethod::L1;
            break;
        case L2:
            return opennn::LossIndex::RegularizationMethod::L2;
            break;
        case NONE:
        default:
            return opennn::LossIndex::RegularizationMethod::NoRegularization;
            break;
        }
    }

} // namespace nerlnet