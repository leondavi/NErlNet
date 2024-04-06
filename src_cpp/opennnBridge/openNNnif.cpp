#include "openNNnif.h"
#define NERLNIF_PREFIX "[NERLNIF] "

void* trainFun(void* arg)
{
    std::shared_ptr<TrainNN>* pTrainNNptr = static_cast<shared_ptr<TrainNN>*>(arg);
    std::shared_ptr<TrainNN> TrainNNptr = *pTrainNNptr;
    delete pTrainNNptr;

    double loss_val;
    ErlNifEnv *env = enif_alloc_env();    
    ERL_NIF_TERM nerlnif_atom = enif_make_atom(env, NERLNIF_ATOM_STR);

    //cout << "TrainNNptr->data = " << *(TrainNNptr->data) << endl;
   // data_set.set_data(*(TrainNNptr->data));

    //get nerlworker from bridge controller
    BridgeController &bridge_controller = BridgeController::GetInstance();
    std::shared_ptr<NerlWorker> nerlworker = bridge_controller.getModelPtr(TrainNNptr->mid);
    std::shared_ptr<NerlWorkerOpenNN> nerlworker_opennn = std::static_pointer_cast<NerlWorkerOpenNN>(nerlworker);
    //get neural network from nerlworker
    std::shared_ptr<opennn::DataSet> data_set_ptr = std::make_shared<opennn::DataSet> ();
    std::shared_ptr<opennn::NeuralNetwork> neural_network_ptr = nerlworker_opennn->get_neural_network_ptr();
    nerlworker_opennn->set_dataset(data_set_ptr, TrainNNptr->data);
    data_set_ptr = nerlworker_opennn->get_data_set();
    std::shared_ptr<TrainingStrategy> training_strategy_ptr = nerlworker_opennn->get_training_strategy_ptr();
    training_strategy_ptr->set_data_set_pointer(nerlworker_opennn->get_dataset_ptr().get());
    TrainingResults res = training_strategy_ptr->perform_training();
    nerlworker_opennn->post_training_process(TrainNNptr->data); 
    loss_val = res.get_training_error(); // learn about "get_training_error" of opennn
    // Stop the timer and calculate the time took for training
    high_resolution_clock::time_point  stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - TrainNNptr->start_time);

    ERL_NIF_TERM train_res_and_time;
    ERL_NIF_TERM train_time = enif_make_double(env, duration.count());

    if(isnan(loss_val)) 
    {
        ERL_NIF_TERM loss_val_term;
        loss_val_term = enif_make_atom(env , NERLNIF_NAN_ATOM_STR);
        cout << NERLNIF_PREFIX << "loss val = nan , setting NN weights to random values" <<std::endl;
        neural_network_ptr->set_parameters_random();// TODO investigate this approache
        train_res_and_time = enif_make_tuple(env, 3 , nerlnif_atom , loss_val_term , train_time);

    }
    else {
        fTensor2DPtr loss_val_tensor = std::make_shared<fTensor2D>(1, 1); // allocate tensor for loss value
        (*loss_val_tensor)(0, 0) = static_cast<float>(loss_val); // set loss value to tensor
        nifpp::TERM loss_val_tensor_term; // allocate erl term for loss value tensor
        nifpp::make_tensor_2d<float,fTensor2D>(env, loss_val_tensor_term, loss_val_tensor);
        train_res_and_time = enif_make_tuple(env, 4 , nerlnif_atom , loss_val_tensor_term , nifpp::make(env, TrainNNptr->return_tensor_type), train_time);
    }

    if(enif_send(NULL,&(TrainNNptr->pid), env, train_res_and_time)){
        //  printf("enif_send train succeed\n");
    }
    else 
    {
        LogError << "enif_send failed " << endl;
        LogError << NERLNIF_PREFIX << "loss val:" << loss_val<< endl;
        LogError << NERLNIF_PREFIX << " train_time:" <<  train_time<< endl;
    }

    return 0;
}

void* PredictFun(void* arg)
{ 
    std::shared_ptr<PredictNN>* pPredictNNptr = static_cast<shared_ptr<PredictNN>*>(arg);
    std::shared_ptr<PredictNN> PredictNNptr = *pPredictNNptr;
    delete pPredictNNptr;
    nifpp::TERM prediction;
    int EAC_prediction; 
    ErlNifEnv *env = enif_alloc_env();    
    //get nerlworker from bridge controller
    BridgeController &bridge_controller = BridgeController::GetInstance();
    std::shared_ptr<NerlWorker> nerlworker = bridge_controller.getModelPtr(PredictNNptr->mid);
    std::shared_ptr<NerlWorkerOpenNN> nerlworker_opennn = std::static_pointer_cast<NerlWorkerOpenNN>(nerlworker);
    //get neural network from nerlworker
    std::shared_ptr<opennn::NeuralNetwork> neural_network = nerlworker_opennn->get_neural_network_ptr();

    Index num_of_samples = PredictNNptr->data->dimension(0);
    Index inputs_number = neural_network->get_inputs_number();
    fTensor2DPtr calculate_res = std::make_shared<fTensor2D>(num_of_samples, neural_network->get_outputs_number());
    Tensor<Index, 1> input_variable_dimension(4);
    Tensor<Index, 1> inputs_dimensions(2);

    if(neural_network->has_convolutional_layer())
    {  
        ConvolutionalLayer* conv = (ConvolutionalLayer*)neural_network->get_layer_pointer(0);
        input_variable_dimension.setValues({num_of_samples,conv->get_input_variables_dimensions()(1), conv->get_input_variables_dimensions()(2), conv->get_input_variables_dimensions()(3)});
        *calculate_res = neural_network->calculate_outputs(PredictNNptr->data->data(), input_variable_dimension);
    }else{
        inputs_dimensions.setValues({num_of_samples, inputs_number});
         *calculate_res = neural_network->calculate_outputs(PredictNNptr->data->data(), inputs_dimensions);
    }
    nerlworker_opennn->post_predict_process(calculate_res); 
    nifpp::make_tensor_2d<float,fTensor2D>(env, prediction, calculate_res);
    // only for AE and AEC calculate the distance between prediction labels and input data
    //std::cout << "*calculate_res.get(): " << (*calculate_res.get()).dimensions() << std::endl;
    // Stop the timer and calculate the time took for training
    high_resolution_clock::time_point  stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - PredictNNptr->start_time);

    ERL_NIF_TERM predict_time = enif_make_double(env, duration.count());
    nifpp::str_atom nerlnif_atom_str(NERLNIF_ATOM_STR);
    nifpp::TERM nerlnif_atom = nifpp::make(env , nerlnif_atom_str);
    ERL_NIF_TERM predict_res_and_time = enif_make_tuple(env, 4 , nerlnif_atom , prediction , nifpp::make(env, PredictNNptr->return_tensor_type) , predict_time);


    if(enif_send(NULL,&(PredictNNptr->pid), env, predict_res_and_time)){
        // printf("enif_send succeed prediction\n");
    }
    else
    {
        LogError << "enif_send failed " << endl;
    }
    return 0;
}