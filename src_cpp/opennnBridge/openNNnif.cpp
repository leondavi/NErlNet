#include "openNNnif.h"
#define NERLNIF_PREFIX "[NERLNIF] "

void* trainFun(void* arg)
{
    std::shared_ptr<TrainNN>* pTrainNNptr = static_cast<shared_ptr<TrainNN>*>(arg);
    std::shared_ptr<TrainNN> TrainNNptr = *pTrainNNptr;
    delete pTrainNNptr;

    double loss_val;
    ErlNifEnv *env = enif_alloc_env();    
    DataSet data_set;

    //get nerlworker from bridge controller
    BridgeController &bridge_controller = BridgeController::GetInstance();
    std::shared_ptr<NerlWorker> nerlworker = bridge_controller.getModelPtr(TrainNNptr->mid);
    std::shared_ptr<NerlWorkerOpenNN> nerlworker_opennn = std::static_pointer_cast<NerlWorkerOpenNN>(nerlworker);
    //get neural network from nerlworker
    std::shared_ptr<opennn::NeuralNetwork> neural_network_ptr = nerlworker_opennn->get_neural_network_ptr();

    int data_cols = TrainNNptr->data->dimension(1);
    int num_of_features = neural_network_ptr->get_inputs_number();

    int num_of_output_neurons = neural_network_ptr->get_outputs_number();
    // cout << "Features: " << num_of_features <<std::endl;
    // cout << "Outputs: " << num_of_output_neurons <<std::endl;
    // cout << "NN got: " << data_cols <<std::endl;
    bool data_set_condition = (num_of_features + num_of_output_neurons) == TrainNNptr->data->dimension(1);
    assert(("issue with data input/output dimensions", data_set_condition));
    data_set.set_data(*(TrainNNptr->data));
    // cout << "Data is set"<<std::endl;
    data_set.set(TrainNNptr->data->dimension(0), num_of_features, num_of_output_neurons);
    // cout << "Configed size"<<std::endl;
    // cout << "Data is: " << *(TrainNNptr->data) <<std::endl;

    std::shared_ptr<TrainingStrategy> training_strategy_ptr = nerlworker_opennn->get_training_strategy_ptr();
    training_strategy_ptr->set_data_set_pointer(&data_set);

    TrainingResults res = training_strategy_ptr->perform_training();
    loss_val = res.get_training_error(); // learn about "get_training_error" of opennn

    //cout << "training done"<<std::endl;

    // Stop the timer and calculate the time took for training
    high_resolution_clock::time_point  stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - TrainNNptr->start_time);

    if(isnan(loss_val)  ) 
    {
        loss_val = -1.0;
        cout << NERLNIF_PREFIX << "loss val = nan , setting NN weights to random values" <<std::endl;
        neural_network_ptr->set_parameters_random();
    }
    //cout << "returning training values"<<std::endl;
    ERL_NIF_TERM loss_val_term = enif_make_double(env, loss_val);
    ERL_NIF_TERM train_time = enif_make_double(env, duration.count());

    ERL_NIF_TERM train_res_and_time = enif_make_tuple(env, 2, loss_val_term,train_time);

    if(enif_send(NULL,&(TrainNNptr->pid), env,train_res_and_time)){
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
    Tensor<Index, 1> inputs_dimensions(2);

    inputs_dimensions.setValues({num_of_samples, inputs_number});

    *calculate_res = neural_network->calculate_outputs(PredictNNptr->data->data(), inputs_dimensions);
    nifpp::make_tensor_2d<float,fTensor2D>(env, prediction, calculate_res);
    
    // Stop the timer and calculate the time took for training
    high_resolution_clock::time_point  stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - PredictNNptr->start_time);
    nifpp::TERM train_time = nifpp::make(env, duration.count());
    std::vector<nifpp::TERM> return_list = {prediction , nifpp::make(env, PredictNNptr->return_tensor_type),train_time};

    if(enif_send(NULL,&(PredictNNptr->pid), env, nifpp::make(env, return_list))){
        // printf("enif_send succeed prediction\n");
    }
    else
    {
        LogError << "enif_send failed " << endl;
    }
    return 0;
}