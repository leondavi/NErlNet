#include "openNNnif.h"
#define NERLNIF_PREFIX "[NERLNIF] "

inline bool set_optimization_method(TrainingStrategy &training_strategy, shared_ptr<TrainNN> TrainNNptr)
{
    int optimization_method = TrainNNptr->optimization_method;
    if(optimization_method == E_OM_GRADIENT_DESCENT){
            training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
            training_strategy.get_stochastic_gradient_descent_pointer()->set_initial_learning_rate(TrainNNptr->learning_rate);
        }
    else if(optimization_method == E_OM_CONJUGATE_GRADIENT)
        {
            training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::CONJUGATE_GRADIENT);
        }
    else if(optimization_method == E_OM_QUASI_NEWTON_METHOD)
        {
            training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::QUASI_NEWTON_METHOD);
        }
    else if(optimization_method == E_OM_LEVENBERG_MARQUARDT_ALGORITHM)
        {
            training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::LEVENBERG_MARQUARDT_ALGORITHM);
        }
    else if(optimization_method == E_OM_STOCHASTIC_GRADIENT_DESCENT)
        {
            training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::STOCHASTIC_GRADIENT_DESCENT);
        }
    else if(optimization_method == E_OM_ADAPTIVE_MOMENT_ESTIMATION)
        {
            training_strategy.set_optimization_method(TrainingStrategy::OptimizationMethod::ADAPTIVE_MOMENT_ESTIMATION);
            training_strategy.get_adaptive_moment_estimation_pointer()->set_initial_learning_rate(TrainNNptr->learning_rate);
        }
    else
    {
        LogError << NERLNIF_PREFIX << "TrainNNptr learning_rate: " << TrainNNptr->learning_rate <<endl; 
        LogError << NERLNIF_PREFIX << "TrainNNptr learning_rate: " << TrainNNptr->learning_rate <<endl; 
        LogError << NERLNIF_PREFIX << "TrainNNptr loss method: " << TrainNNptr->loss_method <<endl; 
        LogError << NERLNIF_PREFIX << "TrainNNptr optimization_method: " << TrainNNptr->optimization_method <<endl; 
        LogError << NERLNIF_PREFIX << "TrainNNptr tid: " << TrainNNptr->tid <<endl;
        LogError << NERLNIF_PREFIX << "optimization_method not choosen " <<std::endl; //TODO create logger
        return false;
    }
    return true;
}

inline bool set_loss_method(TrainingStrategy &training_strategy, shared_ptr<TrainNN> TrainNNptr)
{
    if(TrainNNptr->loss_method == E_LOSS_METHOD_SUM_SQUARED_ERROR){
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::SUM_SQUARED_ERROR);
    }
    else if(TrainNNptr->loss_method == E_LOSS_METHOD_MSE)
    {
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::MEAN_SQUARED_ERROR);
    }
    else if(TrainNNptr->loss_method == E_LOSS_METHOD_NSE)
    {
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::NORMALIZED_SQUARED_ERROR);
    }
    else if(TrainNNptr->loss_method == E_LOSS_METHOD_MINKOWSKI_ERROR)
    {
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::MINKOWSKI_ERROR);
    }
    else if(TrainNNptr->loss_method == E_LOSS_METHOD_WSE)
    {
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::WEIGHTED_SQUARED_ERROR);
    }
    else if(TrainNNptr->loss_method == E_LOSS_METHOD_CEE)
    {
        training_strategy.set_loss_method(TrainingStrategy::LossMethod::CROSS_ENTROPY_ERROR );   
    }
    else
    {
        return false;
    }
    return true;
}

void* trainFun(void* arg)
{
    std::shared_ptr<TrainNN>* pTrainNNptr = static_cast<shared_ptr<TrainNN>*>(arg);
    std::shared_ptr<TrainNN> TrainNNptr = *pTrainNNptr;
    delete pTrainNNptr;

    double loss_val;
    ErlNifEnv *env = enif_alloc_env();    
    DataSet data_set;

    // Get the singleton instance and get the model ID
    opennnBridgeController &onnBrCtrl = opennnBridgeController::GetInstance();
    std::shared_ptr<OpenNN::NeuralNetwork> neural_network = onnBrCtrl.getModelPtr(TrainNNptr->mid);

    int data_cols = TrainNNptr->data->dimension(1);
    int num_of_features = neural_network->get_inputs_number();

    int num_of_output_neurons = neural_network->get_outputs_number();
    // cout << "Features: " << num_of_features <<std::endl;
    // cout << "Outputs: " << num_of_output_neurons <<std::endl;
    // cout << "NN has: " << data_cols <<std::endl;
    bool data_set_condition = (num_of_features + num_of_output_neurons) == TrainNNptr->data->dimension(1);
    assert(("issue with data input/output dimensions", data_set_condition));
    data_set.set_data(*(TrainNNptr->data));
    cout << "Data is set"<<std::endl;
    data_set.set(TrainNNptr->data->dimension(0), num_of_features, num_of_output_neurons);
    cout << "Configed size"<<std::endl;
    //cout << "Data is: " << *(TrainNNptr->data) <<std::endl;

    TrainingStrategy training_strategy;
    training_strategy.set_neural_network_pointer(neural_network.get()); // The order of these two lines is important
    training_strategy.set_data_set_pointer(&data_set);

    assert((set_optimization_method(training_strategy, TrainNNptr), "Issue with set optimization method"));
    assert((set_loss_method(training_strategy, TrainNNptr), "Issue with set loss method"));

    //training_strategy.set_maximum_epochs_number(TrainNNptr->epoch);
    training_strategy.set_maximum_epochs_number(1);
    training_strategy.set_display(TRAINING_STRATEGY_SET_DISPLAY_OFF); // remove opennn prints

    TrainingResults res = training_strategy.perform_training();
    loss_val = res.get_training_error();

    cout << "training done"<<std::endl;

    // Stop the timer and calculate the time took for training
    high_resolution_clock::time_point  stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - TrainNNptr->start_time);

    if(isnan(loss_val)  ) 
    {
        loss_val = -1.0;
        cout << NERLNIF_PREFIX << "loss val = nan , setting NN weights to random values" <<std::endl;
        neural_network->set_parameters_random();
    }
    cout << "returning training values"<<std::endl;
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

    ERL_NIF_TERM prediction;
    int EAC_prediction; 
    ErlNifEnv *env = enif_alloc_env();    
    opennnBridgeController &s = opennnBridgeController::GetInstance();
    std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s.getModelPtr(PredictNNptr->mid);

    int modelType = s.getModelType(PredictNNptr->mid); 
    std::shared_ptr<Eigen::Tensor<float,2>> calculate_res = std::make_shared<Eigen::Tensor<float,2>>();
    *calculate_res = neural_network->calculate_outputs( *(PredictNNptr->data));
    prediction = nifpp::makeTensor2D(env, *calculate_res);
    
    if(enif_send(NULL,&(PredictNNptr->pid), env, prediction)){
        // printf("enif_send succeed prediction\n");
    }
    else printf("enif_send failed\n");
    return 0;
}



/********************************************************************************/
/********************************************************************************/
//                         |
// REFACTOR OR DEPRACATION V
/********************************************************************************/
/********************************************************************************/


void* trainFunAE(void* arg){ 
        bool flag = true;
        std::shared_ptr<TrainNN>* pTrainNNptr = static_cast<shared_ptr<TrainNN>*>(arg);
        std::shared_ptr<TrainNN> TrainNNptr = *pTrainNNptr;
        delete pTrainNNptr;
        // cout << "TrainNNptr learning_rate: " << TrainNNptr->learning_rate <<endl; 
        // cout << "TrainNNptr loss_method: " << TrainNNptr->loss_method <<endl; 
        int optimization_method = TrainNNptr->optimization_method ;
        // cout << "TrainNNptr tid: " << TrainNNptr->tid <<endl; 

         double loss_val;
         ErlNifEnv *env = enif_alloc_env();    
         DataSet data_set;

         
         // Get the singleton instance
         opennnBridgeController& onnBrCtrl = opennnBridgeController::GetInstance();
         std::shared_ptr<OpenNN::NeuralNetwork> neural_network = onnBrCtrl.getModelPtr(TrainNNptr->mid);
       
         int modelType = onnBrCtrl.getModelType(TrainNNptr->mid);
      
         // pointers to autoencoder data
         std::shared_ptr<Eigen::Tensor<float,2>> autoencoder_data;
         std::shared_ptr<Eigen::Tensor<float,2>> data_temp = std::make_shared<Eigen::Tensor<float,2>>();
    
         // inishalize autoencoder data

         int data_num_of_cols = TrainNNptr->data->dimension(1);
         int NN_input_num = neural_network->get_inputs_number();

         if (modelType == E_AE || modelType == E_AEC){
            Eigen::array<int, 2> bcast({1, 2});  
            autoencoder_data = std::make_shared<Eigen::Tensor<float,2>>(TrainNNptr->data->broadcast(bcast));                 
 
            if(modelType == E_AE){    
              data_set.set_data(*autoencoder_data);
              data_set.set(autoencoder_data->dimension(1),data_num_of_cols,data_num_of_cols);
            }
         }
          
         // inishalize NN data (not autoencoder data)
         else {
            try{
                data_set.set_data(*(TrainNNptr->data));
            }
            catch(...) { 
                flag = false;
                std::cout << "catch set_data " <<std::endl; 
                }
         }


          TrainingStrategy training_strategy;
          if(flag == true) 
          {
            training_strategy.set_neural_network_pointer(neural_network.get()); // The order of these two lines is important
            training_strategy.set_data_set_pointer(&data_set);
          }
        
        assert((set_optimization_method(training_strategy, TrainNNptr), "Issue with set optimization method"));
        assert((set_loss_method(training_strategy, TrainNNptr), "Issue with set loss method"));

         training_strategy.set_maximum_epochs_number(1); 
    
         training_strategy.set_display(TRAINING_STRATEGY_SET_DISPLAY_OFF);
         
         // EAC train 
         if(modelType == E_AEC){
            *data_temp = *(TrainNNptr->data);
            int autoencoder_data_num_of_cols = autoencoder_data->dimension(1);
            
            if(data_num_of_cols == 256 && autoencoder_data_num_of_cols == 512 && flag == true){
                std::shared_ptr<AutoencoderClassifier> Autoencoder_Classifier = std::static_pointer_cast<AutoencoderClassifier>(neural_network);
                loss_val = Autoencoder_Classifier->train(autoencoder_data, data_temp);//, neural_network);
            }

            else{
                loss_val = 1;
                cout << "data set error  " <<std::endl; 
                cout << "data_num_of_cols " << data_num_of_cols << std::endl; 
                cout << "autoencoder_data_num_of_cols " << autoencoder_data_num_of_cols << std::endl;
                cout << "flag  " << flag << std::endl;
            }
         }

         else{
 
             if(data_num_of_cols == (NN_input_num+1) && flag == true){
              
                TrainingResults  res = training_strategy.perform_training();
              

               loss_val = res.get_training_error();
            
           
                 
         // do NN trainig
         //chech the inputs from erlang and neural network architecture ---------------------------------------------------
         //TestingAnalysis testing_analysis(&*neural_network, &data_set);
         //TestingAnalysis testing_analysis;
    /*
                TestingAnalysis testing_analysis;
                //std::shared_ptr<OpenNN::TestingAnalysis> testing_analysis =  std::make_shared<OpenNN::TestingAnalysis>();
                cout << "8181818181" <<std::endl; 
                testing_analysis.set_neural_network_pointer(&*neural_network);
                cout << "8282828282" <<std::endl; 
                testing_analysis.set_data_set_pointer(&data_set);
                cout << "8383838383" <<std::endl; 

            try{ 
               
                Tensor< type, 2 > calculate_errors = testing_analysis.calculate_errors();
            Tensor<type, 1> testing_errors = testing_analysis.calculate_testing_errors();
            //sse - testing_errors[0]
            //mse - testing_errors[1]
            //root mse - testing_errors[2]
            //nse - testing_errors[3]
            ERL_NIF_TERM error = nifpp::makeTensor1D(env, testing_errors);
            if(TrainNNptr->loss_method == E_LOSS_METHOD_SUM_SQUARED_ERROR) loss_val = testing_errors[0]; 
            else if(TrainNNptr->loss_method == E_LOSS_METHOD_MSE)          loss_val = testing_errors[1];  
            else if(TrainNNptr->loss_method == E_LOSS_METHOD_NSE)          loss_val = testing_errors[3]; 
            else loss_val = testing_errors[1];
            loss_val = calculate_errors(0,0);
            
            } 
            catch(...){
                cout << "catch - calculate errors" <<std::endl;
            } 
            */
           }
               else { 
                cout << "data set error  " <<std::endl; 
                cout << "data_num_of_cols " << data_num_of_cols << std::endl; 
                cout << "NN input num " << NN_input_num << std::endl; 
                cout << "flag  " << flag << std::endl;
                }
         }
   
         // Stop the timer and calculate the time took for training
         high_resolution_clock::time_point  stop = high_resolution_clock::now();
         auto duration = duration_cast<microseconds>(stop - TrainNNptr->start_time);
      
         if(isnan(loss_val)  ) {
             loss_val = -1.0;
             cout << "loss val = nan , setting NN weights to random values" <<std::endl;
             neural_network->set_parameters_random();
         }
      
         ERL_NIF_TERM loss_val_term = enif_make_double(env, loss_val);
         ERL_NIF_TERM train_time = enif_make_double(env, duration.count());
        
         ERL_NIF_TERM train_res_and_time = enif_make_tuple(env, 2, loss_val_term,train_time);
        
        if(enif_send(NULL,&(TrainNNptr->pid), env,train_res_and_time)){
            //  printf("enif_send train succeed\n");
        }
        else {
            cout << "loss val:" << loss_val<< endl;
            cout << " train_time:" <<  train_time<< endl;
            printf("enif_send failed\n");
        }
        
         
         return 0;
}

void* PredictFunAE(void* arg)
{ 
    std::shared_ptr<PredictNN>* pPredictNNptr = static_cast<shared_ptr<PredictNN>*>(arg);
    std::shared_ptr<PredictNN> PredictNNptr = *pPredictNNptr;
    delete pPredictNNptr;

    ERL_NIF_TERM prediction;
    int EAC_prediction; 
    ErlNifEnv *env = enif_alloc_env();    
    opennnBridgeController& s = opennnBridgeController::GetInstance();
    std::shared_ptr<OpenNN::NeuralNetwork> neural_network = s.getModelPtr(PredictNNptr->mid);
        //   cout << "222222222222" << endl;

    int modelType = s.getModelType(PredictNNptr->mid); 
    std::shared_ptr<Eigen::Tensor<float,2>> calculate_res = std::make_shared<Eigen::Tensor<float,2>>();
    *calculate_res = neural_network->calculate_outputs( *(PredictNNptr->data));
    //   cout << "33333333333333" << endl;

    if(modelType == E_AEC){
        
        std::shared_ptr<AutoencoderClassifier> Autoencoder_Classifier = std::static_pointer_cast<AutoencoderClassifier>(neural_network);
        iTensor1DPtr predictRes  = Autoencoder_Classifier->predict(PredictNNptr->data);
        prediction = nifpp::makeTensor1D(env, (*predictRes));
    }
    else
        prediction = nifpp::makeTensor2D(env, *calculate_res);
        
            // cout << "44444444444" << endl;
    if(enif_send(NULL,&(PredictNNptr->pid), env, prediction)){
    // printf("enif_send succeed prediction\n");
    }
    else printf("enif_send failed\n");
    return 0;
}