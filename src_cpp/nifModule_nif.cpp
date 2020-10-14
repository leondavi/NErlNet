// g++ -fno-rtti -o nifModule_nif.so -fpic -shared test.cpp nifModule_nif.cpp

#include "include/nifpp.h"
#include <vector>
#include <deque>
#include "include/cppSANN/src/tests/default_test.h"
//#include "include/cppSANN/src/tests/api.h"
#include <thread>

// Codes for the module state
enum ModuleMode {CREATE = 0, TRAIN = 1, PREDICT = 2};

// ----- Structs ------

// Create model parameters struct
// layers_sizes - list in erlang. represents the layers sizes of the neural network
// Learning_rate - number (0-1). Usually 1/number_of_samples
// Train_set_size - percentage number. Usually 70%-80%. Represents the portion of the data that we train
// Activation_list (optional) - list
// Optimizer (optional) - default ADAM
// tid - unique thread identification
struct CreateModelParam {

    int optimizer;
    double learning_rate, train_set_size;
    std::vector<int> activation_list;
    std::vector<uint32_t> layers_sizes;
    ErlNifTid tid;
};

// Train mode parameters struct
// rows, col - Represents the rows and columns of the data matrix
// labels - Represents the label portion of tha label matrix from the data_label matrix
// tid - unique thread identification
// data_Label_mat - list in erlang. Represents tha data and the label matrix (last columns) together
struct TrainParam {

    int rows, col, labels;
    std::vector<double> data_label_mat;
    ErlNifTid tid;
};

// Predict mode parameters struct
// data_mat - The data matrix without the labels
// tid - unique thread identification
struct PredictParam {

    int rows, cols;
    std::vector<double> data_mat;
    ErlNifTid tid;
};


struct MidStruct {

    SANN::Model model;
    ErlNifTid tid;
    //TODO: add PID
};

//-------------------------------------------


// Neural network manager singleton
class nnManager {
private:
    static nnManager *instance;
    //static std::mutex mutex_;
protected:
    //~nnManager() {}
    //std::unordered_map<int, int> midTidMap; // <Mid,Pid> - Model id, process id
    //std::unordered_map<int, int> pidTidMap; // <Pid,Tid> - Process id, Thread id
    std::unordered_map<int, SANN::Model*> MidNumModel; // <Mid,Model struct>
    int data;


    nnManager() {
        data = 0;
    }

public:
    /**
     * Singletons should not be cloneable.
     */
    nnManager(nnManager &other) = delete;
    /**
     * Singletons should not be assignable.
     */
    void operator=(const nnManager &) = delete;

   /* nnManager(data)
    {
	    if (instance == nullptr)
	    {
		//std::lock_guard<std::mutex> lock(mutex_);
		//if (instance == nullptr)
		//{
		    instance = new nnManager();
		//}
	    }
	    return instance;
    }*/


    static nnManager *GetInstance();

    int getData() {
        return this -> data;
    }

    SANN::Model* getModelPtr(int data){
        return this->MidNumModel[data];
    }

    void setData(SANN::Model * modelPtr) {
        this -> MidNumModel.insert({ this->data, modelPtr });
        this -> data = this->data + 1;
    }
};

/**
 * Static methods should be defined outside the class.
 */
//Initialize pointer to zero so that it can be initialized in first call to getInstance
nnManager* nnManager::instance{nullptr};
//std::mutex nnManager::mutex_;

/**
 * The first time we call GetInstance we will lock the storage location
 *      and then we make sure again that the variable is null and then we
 *      set the value. RU:
 */
nnManager *nnManager::GetInstance()
{
    if (instance == nullptr)
    {
        //std::lock_guard<std::mutex> lock(mutex_);
        //if (instance == nullptr)
        //{
            instance = new nnManager();
        //}
    }
    return instance;
}

class GetnnManager {

    nnManager *s;
public:
    GetnnManager() {
        s = s->GetInstance();
    }

};

// All terms of type ERL_NIF_TERM belong to an environment of type ErlNifEnv.
// The lifetime of a term is controlled by the lifetime of its environment object.
// All API functions that read or write terms has the environment that the term belongs to as the first function argument.
static ERL_NIF_TERM nnManager_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    try
    {
        auto s_ptr = nifpp::construct_resource<GetnnManager>();
        return nifpp::make(env, s_ptr);
    }
    catch(nifpp::badarg) {}
    //catch(std::ios_base::failure) {}
    return enif_make_badarg(env);

	//return nifpp::make(env, s);
}



static ERL_NIF_TERM nnManagerGetData_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nnManager *s = s->GetInstance();
    int data = s->getData();

    return enif_make_int(env, data);
}

static ERL_NIF_TERM nnManagerGetModelPtr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int mid;

    if (!enif_get_int(env, argv[0], &mid)) {
        return enif_make_badarg(env);
    }

    nnManager *s = s->GetInstance();
    SANN::Model* model = s->getModelPtr(mid);

    return enif_make_int(env, mid); // todo: change to something else
}


static ERL_NIF_TERM nnManagerSetData_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nnManager *s = s->GetInstance();
    int data;

    if (!enif_get_int(env, argv[0], &data)) {
	return enif_make_badarg(env);
    }
   // s->setData(data);

    return enif_make_int(env, data);
}

// ---------------------------------------------------

// Predict function - runs on a separate thread (thread_create)
static void* predictFun(void *arg){

    PredictParam* predictPtr = (PredictParam*)arg;

    // Get the singleton instance
    nnManager *s = s->GetInstance();

    // Get the model from the singleton
    SANN::Model* modelPtr = s-> getModelPtr(0); // TODO: make it general, change the 0

    // Get the data(predict) matrix from the data_label_mat vector and initialize the data matrix. TODO: Think how to do it native to eigen
    MatrixXd data_matrix(predictPtr->rows, predictPtr->cols); // TODO: send the col and rows

    // Debug print
    /*std::cout<<"rows: \n"<<predictPtr->rows<<std::endl;
    std::cout<<"cols: \n"<<predictPtr->cols<<std::endl;
    std::cout<<"data_matrix: \n"<<std::endl;*/

    int i = 0;
    for (int r = 0; r < predictPtr->rows; r++){
        for (int c = 0; c < predictPtr->cols; c++){
            data_matrix(r,c) = predictPtr->data_mat[i];
            //std::cout<<"("<<r<<","<<c<<") - "<< data_matrix(r,c) <<std::endl; // Debug print
            i++;
        }
    }

    // Predict model with received parameters
    MatrixXd results = modelPtr->predict(data_matrix); //todo: change data_with_noise

    //TODO: update the result matrix in a visible place

    std::cout<<"results: \n"<<results<<std::endl;

    printf("finish predict fun.\n");

    return 0;
}

// Train function - runs on a separate thread (thread_create)
static void* trainFun(void *arg){

    TrainParam* trainPtr = (TrainParam*)arg;

    // Get the singleton instance
    nnManager *s = s->GetInstance();

    // Get the model from the singleton
    SANN::Model* modelPtr = s-> getModelPtr(0); // TODO: make it general, change the 0

    // Get the data matrix from the data_label_mat vector and initialize the data matrix. TODO: Think how to do it native to eigen
    MatrixXd data_mat(trainPtr->rows, trainPtr->col);

    // Debug print
    /*std::cout<<"rows: \n"<<trainPtr->rows<<std::endl;
    std::cout<<"cols: \n"<<trainPtr->col<<std::endl;
    std::cout<<"data_mat: \n"<<std::endl;
    */

    int i = 0;
    for (int r = 0; r < trainPtr->rows; r++){
        for (int c = 0; c < trainPtr->col; c++){
            data_mat(r,c) = trainPtr->data_label_mat[i];
            //std::cout<<"("<<r<<","<<c<<") - "<< data_mat(r,c) <<std::endl; // Debug print
            i++;
        }
    }

    // Get the label matrix from the data_label_mat vector and initialize the label matrix
    MatrixXd label_mat(trainPtr->rows, trainPtr->labels);

    //std::cout<<"label_mat: \n"<<std::endl; // Debug print

    int j = 0;
    for (int r = 0; r < trainPtr->rows; r++){
        for (int c = 0; c < trainPtr->labels; c++){
            label_mat(r,c) = trainPtr->data_label_mat[i+j]; // TODO: correct it
            //std::cout<<"("<<r<<","<<c<<") - "<< label_mat(r,c) <<std::endl; // Debug print
            j++;
        }
    }

    // Train the model with recieved parameters
    modelPtr->train(data_mat,label_mat,false); // TODO: the false parameter is redundant?

    printf("finish train fun.\n");

    return 0;
}

// Train, Predict and Create module nif
static ERL_NIF_TERM train_predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // mode = 0 - model creation, 1 - train, 2 - predict
    int mode;

    // Get mode (int) from erlang term
    if (!enif_get_int(env, argv[0], &mode)) {
        return enif_make_badarg(env);
    }

    // Create model - 0
    if(mode == CREATE){

       // CreateModelParam* modelParamPtr1 = new CreateModelParam;
        //std::shared_ptr<CreateModelParam> modelParamPtr(modelParamPtr1);

        CreateModelParam modelParamPtr = CreateModelParam();

        // Create a struct to store the parameters for module create
        //CreateModelParam* modelParamPtr = new CreateModelParam(); //TODO: IN the future change to shared/unique pointers
        //std::vector<uint32_t> layers_sizes;

        try{
            // Convert the erlang nif types to C++ types
            // get_throws is a template wrapper for enif_get_XXX(). Provides the option to use c++ types.
            // It will throw nifpp::badarg upon failure unlike nifpp::get() that will return true on success, false on failure.
            nifpp::get_throws(env, argv[1], modelParamPtr.layers_sizes);
            nifpp::get_throws(env, argv[2], modelParamPtr.learning_rate);
            nifpp::get_throws(env, argv[3], modelParamPtr.train_set_size);
            nifpp::get_throws(env, argv[4], modelParamPtr.activation_list); // optional
            nifpp::get_throws(env, argv[5], modelParamPtr.optimizer); // optional

            // Default parameter. TODO: Will be changed in the future
            std::vector<act_t> act_types_vec{act_t::ACT_NONE,act_t::ACT_SIGMOID,act_t::ACT_SIGMOID,act_t::ACT_NONE};

            // Create the model with the default and received parameters
           static SANN::Model model(modelParamPtr.layers_sizes,modelParamPtr.learning_rate); // TODO: Think about the static declaration

            model.set_activations(act_types_vec);
            model.set_optimizer(Optimizers::OPT_ADAM);// The default is Adam optimizer but you can select another

            SANN::Model *modelPtr = &model;

            // Create the singleton instance
            nnManager *s = s->GetInstance();

            // Put the model record to the map
            s->setData(modelPtr);

            printf("finish create module nif.\n");
            return enif_make_int(env, 0); // TODO: change the return value
        }
        catch(nifpp::badarg){
            return enif_make_badarg(env);
        }
    }
    // Train mode - 1
    else if(mode == TRAIN){

        //struct TrainParam train;
        //TrainParam* trainPtr = &train;

        TrainParam* trainPtr = new TrainParam(); //TODO: Delete It and in the future think about changing it to shared/unique pointers/regular stack struct

        try{
            // Convert the erlang nif types to C++ types
            nifpp::get_throws(env, argv[1], trainPtr->rows);
            nifpp::get_throws(env, argv[2], trainPtr->col);
            nifpp::get_throws(env, argv[3], trainPtr->labels);
            nifpp::get_throws(env, argv[4], trainPtr->data_label_mat);

            // int enif_thread_create(char *name, ErlNifTid *tid, void * (*func)(void *), void *args, ErlNifThreadOpts *opts)
            // name -A string identifying the created thread. It is used to identify the thread in planned future debug functionality.
            // tid - A pointer to a thread identifier variable.
            // func - A pointer to a function to execute in the created thread.
            // arg - A pointer to argument to the func function.
            // opts - A pointer to thread options to use or NULL.
            // Returns 0 on success, otherwise an errno value is returned to indicate the error.
            // The newly created thread begins executing in the function pointed to by func, and func is passed arg as argument.
            // When erl_drv_thread_create returns, the thread identifier of the newly created thread is available in *tid.
            // opts can be either a NULL pointer, or a pointer to an ErlDrvThreadOpts structure.
            // If opts is a NULL pointer, default options are used, otherwise the passed options are used.
            int res = enif_thread_create((char*)"trainModule", &(trainPtr->tid), trainFun, trainPtr, 0);

            printf("finish train module nif.\n"); // TODO: add tid to print
            return enif_make_int(env, res);
        }
        catch(nifpp::badarg){
           return enif_make_badarg(env);
        }
    }
    // Predict mode - 2
    else if (mode == PREDICT){

        //struct PredictParam predict;
        //PredictParam* predictPtr = &predict;

        PredictParam* predictPtr = new PredictParam(); //TODO: IN the future change to shared/unique pointers

        // Convert the erlang nif types to C++ types
        nifpp::get_throws(env, argv[1], predictPtr->data_mat);
        nifpp::get_throws(env, argv[2], predictPtr->rows);
        nifpp::get_throws(env, argv[3], predictPtr->cols);

        // Create the thread to run predict
        int res = enif_thread_create((char*)"predictModule", &(predictPtr->tid), predictFun, predictPtr, 0);

        printf("finish predict nif.\n"); // TODO: add tid
        return enif_make_int(env, res);
    }

    return enif_make_int(env, mode);
}


//--------------------------------------

// Describes a NIF by its name in erlang, arity, implementation in c/c++ (ERL_NIF_TERM) and dirty nif flag.
static ErlNifFunc nif_funcs[] = {
    {"nnManagerGetData", 0, nnManagerGetData_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nnManagerGetModelPtr", 1, nnManagerGetModelPtr_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nnManagerSetData", 1, nnManagerSetData_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"train_predict", 5, train_predict_nif,ERL_NIF_DIRTY_JOB_CPU_BOUND}, // For train
    {"train_predict", 4, train_predict_nif,ERL_NIF_DIRTY_JOB_CPU_BOUND}, // For predict
    {"create_module", 6, train_predict_nif,ERL_NIF_DIRTY_JOB_CPU_BOUND}, // For module create
    {"nnManager", 0, nnManager_nif}
};

// load_info is the second argument to erlang:load_nif/2.
// *priv_data can be set to point to some private data if the library needs to keep a state between NIF calls.
// enif_priv_data returns this pointer. *priv_data is initialized to NULL when load is called.
// The library fails to load if load returns anything other than 0. load can be NULL if initialization is not needed.
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    nifpp::register_resource<GetnnManager>(env, nullptr, "GetnnManager");
    nifpp::register_resource<nnManager>(env, nullptr, "nnManager");
    nifpp::register_resource<SANN::Model>(env, nullptr, "nnManager");
    return 0;
}

// This is the magic macro to initialize a NIF library. It is to be evaluated in global file scope.
// ERL_NIF_INIT(MODULE, ErlNifFunc funcs[], load, NULL, upgrade, unload)
// MODULE -  The first argument must be the name of the Erlang module as a C-identifier. It will be stringified by the macro.
// ErlNifFunc - The second argument is the array of ErlNifFunc structures containing name, arity, and function pointer of each NIF.
// load -  is called when the NIF library is loaded and no previously loaded library exists for this module.
// NULL - The fourth argument NULL is ignored. It was earlier used for the deprecated reload callback which is no longer supported since OTP 20.
// The remaining arguments are pointers to callback functions that can be used to initialize the library.
// They are not used in this simple example, hence they are all set to NULL.
ERL_NIF_INIT(erlModule, nif_funcs, load, NULL, NULL, NULL)
