// g++ -fno-rtti -o nifModule_nif.so -fpic -shared test.cpp nifModule_nif.cpp

#include "include/nifpp.h"
#include <vector>
#include <deque>



// -----------Functions---------------
std::vector<double> square(std::vector<double> listVec) {
  
    for(int i=0; i < listVec.size(); ++i) {
       	listVec[i] = listVec[i]*listVec[i];
    } 
    return listVec;
}

int foo(int x) {
  return x+1;
}

//-------------------------------



// All terms of type ERL_NIF_TERM belong to an environment of type ErlNifEnv.
// The lifetime of a term is controlled by the lifetime of its environment object.
// All API functions that read or write terms has the environment that the term belongs to as the first function argument.
static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}


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


//--------------------------------------




// Describes a NIF by its name, arity, and implementation.
static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"nnManagerGetData", 0, nnManagerGetData_nif},
    {"nnManagerSetData", 1, nnManagerSetData_nif},
    {"nnManager", 0, nnManager_nif}
};


static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    nifpp::register_resource<GetnnManager>(env, nullptr, "GetnnManager");
    return 0;
}

// The first argument must be the name of the Erlang module as a C-identifier. It will be stringified by the macro.
// The second argument is the array of ErlNifFunc structures containing name, arity, and function pointer of each NIF.
// The remaining arguments are pointers to callback functions that can be used to initialize the library. They are not used in this simple example, hence they are all set to NULL.
ERL_NIF_INIT(erlModule, nif_funcs, load, NULL, NULL, NULL)
