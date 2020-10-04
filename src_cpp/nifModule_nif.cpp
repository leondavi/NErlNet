// g++ -fno-rtti -o nifModule_nif.so -fpic -shared test.cpp nifModule_nif.cpp

#include "include/nifpp.h"
#include <vector>
#include <deque>
#include "include/cppSANN/src/tests/default_test.h"
#include "include/cppSANN/src/tests/api.h"
//#include "include/cppSANN/src/Models/include/Model.h"
#include <thread>



// -----------Functions---------------
// todo: delete function
std::vector<double> square(std::vector<double> listVec) {
  
    for(int i=0; i < listVec.size(); ++i) {
       	listVec[i] = listVec[i]*listVec[i];
    } 
    return listVec;
}
/*
//-------------------------------



// All terms of type ERL_NIF_TERM belong to an environment of type ErlNifEnv.
// The lifetime of a term is controlled by the lifetime of its environment object.
// All API functions that read or write terms has the environment that the term belongs to as the first function argument.
//static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
//{
 //   int x, ret;
  //  if (!enif_get_int(env, argv[0], &x)) {
//	return enif_make_badarg(env);
 //   }
  //  ret = foo(x);
   // return enif_make_int(env, ret);
//}
*/

//--------------------------------------

// Neural network manager singleton
class nnManager {
private:
    static nnManager *instance;
    //static std::mutex mutex_;
protected:
    ~nnManager() {}
    std::unordered_map<int, int> midTidMap; // <Mid,Pid> - Model id, process id
    std::unordered_map<int, int> pidTidMap; // <Pid,Tid> - Process id, Thread id
    int data;

    nnManager() {
        data = 0;
    }

    //thread_create(model_id) {
    //}

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

    void setData(int data) {
        this -> data = data;
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

static ERL_NIF_TERM nnManager_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	

// nnManager test
  //  nnManager *s = s->GetInstance();
    //cout << s->getData() << endl;
    //s->setData(100);
    //cout << s->getData() << endl;
    //nnManager * s1 = s->GetInstance();
    //cout << s1->getData() << endl;

  	//std::vector<double> listVec, ret;

	/*try{
		nifpp::get_throws(env, argv[0], listVec);
		ret = square(listVec);
		return nifpp::make(env, ret);
    	}
	catch(nifpp::badarg){}*/
	//return enif_make_badarg(env);

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

static ERL_NIF_TERM nnManagerSetData_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nnManager *s = s->GetInstance();
    int data;

    if (!enif_get_int(env, argv[0], &data)) {
	return enif_make_badarg(env);
    }
    s->setData(data);

    return enif_make_int(env, data);
}

int m = 3;

// Predict function
static void* predictFun(void *arg){

    //int * argP;
    //argP = (int *)arg;

    return 0;
}

// Train function
static void* trainFun(void *arg){

    //int * argP;
    //argP = (int *)arg;

    return 0;
}

static ERL_NIF_TERM train_predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // mode = 0 - model creation, 1 - train, 2 - predict
    int mode;

    if (!enif_get_int(env, argv[0], &mode)) {
        return enif_make_badarg(env);
    }

    // Create model
    if(mode == 0){
        //ErlNifTid tid;
        //int *argP = &m;

        int optimizer;
        double learning_rate, train_set_size;
        std::vector<double> layers_sizes, activation_list;
        ERL_NIF_TERM ret_data_mat, ret_label_mat, ret_layers_sizes;

        try{
            nifpp::get_throws(env, argv[1], data_mat);
            nifpp::get_throws(env, argv[2], label_mat);
            nifpp::get_throws(env, argv[3], layers_sizes);
            ret_data_mat = nifpp::make(env, data_mat);
            ret_label_mat = nifpp::make(env, label_mat);
            ret_layers_sizes = nifpp::make(env, layers_sizes);
            return ret_label_mat;
        }
        catch(nifpp::badarg){
           return enif_make_badarg(env);
        }
    }
    // Train mode
    else if(mode == 1){
        //ErlNifTid tid;
        //int *argP = &m;

        std::vector<double> data_mat,label_mat, layers_sizes;
        ERL_NIF_TERM ret_data_mat, ret_label_mat, ret_layers_sizes;

        try{
            nifpp::get_throws(env, argv[1], data_mat);
            nifpp::get_throws(env, argv[2], label_mat);
            nifpp::get_throws(env, argv[3], layers_sizes);
            ret_data_mat = nifpp::make(env, data_mat);
            ret_label_mat = nifpp::make(env, label_mat);
            ret_layers_sizes = nifpp::make(env, layers_sizes);
            return ret_label_mat;
        }
        catch(nifpp::badarg){
           return enif_make_badarg(env);
        }


        //  MatrixXd data_mat(4,8);

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
        //int ret = enif_thread_create(NULL, &tid, nif_worker_fn, argP, NULL);
        int res = enif_thread_create((char*)"train", &tid, trainFun, argP, NULL);
        return enif_make_int(env, res);
        printf("finish train nif.\n");
    }
    // Predict mode
    else if (mode == 2){
        ErlNifTid tid;
        int *argP = &m;

        int res = enif_thread_create((char*)"predict", &tid, predictFun, argP, NULL);
        return enif_make_int(env, res);
        printf("finish predict nif.\n");
    }
    //deafault_test();
    /*nnManager *s = s->GetInstance();
    int data;

    if (!enif_get_int(env, argv[0], &data)) {
        return enif_make_badarg(env);
    }
    s->setData(data);

    return enif_make_int(env, data);
     */

    //printf("Wait for 10 seconds to exit.\n");
    //std::this_thread::sleep_for(std::chrono::seconds(10));

    return enif_make_int(env, mode);
}


static void* nif_worker_fn(void *arg){

    //int * argP;
    //argP = (int *)arg;

    return 0;
}

int t = 3;

// thread_create test
static ERL_NIF_TERM thread_create_test_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    //char* name = "testThread";
    ErlNifTid tid;
    int *argP = &t;


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
    //int ret = enif_thread_create(NULL, &tid, nif_worker_fn, argP, NULL);
    int res = enif_thread_create((char*)"thread", &tid, nif_worker_fn, argP, NULL);
    return enif_make_int(env, res);
}



static ERL_NIF_TERM predict_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    //char* name = "testThread";
    ErlNifTid tid;
    int *argP = &t;


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
    //int ret = enif_thread_create(NULL, &tid, nif_worker_fn, argP, NULL);
    int res = enif_thread_create((char*)"predict", &tid, predictFun, argP, NULL);
    return enif_make_int(env, res);
}

//--------------------------------------




// Describes a NIF by its name in erlang, arity, implementation in c/c++ (ERL_NIF_TERM) and dirty nif flag.
static ErlNifFunc nif_funcs[] = {
    {"nnManagerGetData", 0, nnManagerGetData_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nnManagerSetData", 1, nnManagerSetData_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"train_predict", 3, train_predict_nif,ERL_NIF_DIRTY_JOB_CPU_BOUND}, module_create

    {"module_create", 3, module_create_nif,ERL_NIF_DIRTY_JOB_CPU_BOUND}, // TODO: add module_create_nif

    {"predict", 0, predict_nif,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"thread_create_test", 0, thread_create_test_nif},
    {"nnManager", 0, nnManager_nif}
};

// load_info is the second argument to erlang:load_nif/2.
// *priv_data can be set to point to some private data if the library needs to keep a state between NIF calls.
// enif_priv_data returns this pointer. *priv_data is initialized to NULL when load is called.
// The library fails to load if load returns anything other than 0. load can be NULL if initialization is not needed.
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    nifpp::register_resource<GetnnManager>(env, nullptr, "GetnnManager");
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
