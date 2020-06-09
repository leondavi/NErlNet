// g++ -fno-rtti -o nifModule_nif.so -fpic -shared test.cpp nifModule_nif.cpp

#include "nifpp.h"
#include <vector>
#include <deque>

extern int foo(int x);
extern int bar(int y);
//extern std::vector<T> square(std::vector<T> listVec);
extern std::deque<int> push(std::deque<int> listDeq);
extern std::vector<int> square2(std::vector<int> listVec, std::vector<int> listVec2);


// -----------Functions---------------
std::vector<double> square(std::vector<double> listVec) {
  
    for(int i=0; i < listVec.size(); ++i) {
       	listVec[i] = listVec[i]*listVec[i];
    } 
    return listVec;
}
//-------------------------------



// All terms of type ERL_NIF_TERM belong to an environment of type ErlNifEnv. The lifetime of a term is controlled by the lifetime of its environment object. All API functions that read or write terms has the environment that the term belongs to as the first function argument.
static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int y, ret;
    if (!enif_get_int(env, argv[0], &y)) {
	return enif_make_badarg(env);
    }
    ret = bar(y);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM square_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	
  	std::vector<double> listVec, ret;

	try{
		nifpp::get_throws(env, argv[0], listVec);
		ret = square(listVec);
		return nifpp::make(env, ret);
    	}
	catch(nifpp::badarg){}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM square2_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  	std::vector<int> listVec, listVec2, ret;

	try{
		nifpp::get_throws(env, argv[0], listVec);
		nifpp::get_throws(env, argv[1], listVec2);

		ret = square2(listVec, listVec2);
		return nifpp::make(env, ret);
    	}
	catch(nifpp::badarg){}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM push_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  	std::deque<int> listDeq, ret;
	nifpp::TERM term;

	try{
		nifpp::get_throws(env, argv[0], listDeq);
		ret = push(listDeq);
		term = nifpp::make(env, ret);
		return term;
    	}
	catch(nifpp::badarg){}
	return enif_make_badarg(env);
}
//--------------------------------------

// Singleton
class Singleton {
private:
    static Singleton *instance;
    //static std::mutex mutex_;
protected:
    ~Singleton() {}
    int data;

    Singleton() {
        data = 0;
    }

public:
    /**
     * Singletons should not be cloneable.
     */
    Singleton(Singleton &other) = delete;
    /**
     * Singletons should not be assignable.
     */
    void operator=(const Singleton &) = delete;

    static Singleton *GetInstance();

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
Singleton* Singleton::instance{nullptr};
//std::mutex Singleton::mutex_;

/**
 * The first time we call GetInstance we will lock the storage location
 *      and then we make sure again that the variable is null and then we
 *      set the value. RU:
 */
Singleton *Singleton::GetInstance()
{
    if (instance == nullptr)
    {
        //std::lock_guard<std::mutex> lock(mutex_);
        //if (instance == nullptr)
        //{
            instance = new Singleton();
        //}
    }
    return instance;
}

static ERL_NIF_TERM singleton_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	

// Singleton test
    Singleton *s = s->GetInstance();
    //cout << s->getData() << endl;
    //s->setData(100);
    //cout << s->getData() << endl;
    //Singleton * s1 = s->GetInstance();
    //cout << s1->getData() << endl;

  	//std::vector<double> listVec, ret;

	/*try{
		nifpp::get_throws(env, argv[0], listVec);
		ret = square(listVec);
		return nifpp::make(env, ret);
    	}
	catch(nifpp::badarg){}*/
	//return enif_make_badarg(env);
	return nifpp::make(env, s);
}

//--------------------------------------




// Describes a NIF by its name, arity, and implementation.
static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"bar", 1, bar_nif},
    {"square", 1, square_nif},
    {"square", 2, square2_nif},
    {"singleton", 0, singleton_nif},
    {"push", 1, push_nif}
};

// The first argument must be the name of the Erlang module as a C-identifier. It will be stringified by the macro.
// The second argument is the array of ErlNifFunc structures containing name, arity, and function pointer of each NIF.
// The remaining arguments are pointers to callback functions that can be used to initialize the library. They are not used in this simple example, hence they are all set to NULL.
ERL_NIF_INIT(erlModule, nif_funcs, NULL, NULL, NULL, NULL)
