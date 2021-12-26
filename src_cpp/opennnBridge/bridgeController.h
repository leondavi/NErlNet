#pragma once 

#include <unordered_map>
#include <mutex>
#include <memory>
#include "../../opennn/opennn/opennn.h" // adding the api of opennn for creating a model

#ifndef BUILDSCRIPT_PY_BRIDGECONTROLLER_H
#define BUILDSCRIPT_PY_BRIDGECONTROLLER_H

#endif //BUILDSCRIPT_PY_BRIDGECONTROLLER_H

std::mutex mutex_;
// Neural network manager singleton
class opennnBridgeController {
private:
    static opennnBridgeController *instance;
protected: // Enabling use of the clause only in other classes
    ~opennnBridgeController() {}
    std::unordered_map<unsigned long, std::shared_ptr<OpenNN::NeuralNetwork>> _MidNumModel; // <Mid,Model struct> Dictionary: choosing a model with  model id key
                                                                                  // SANN::Model to OpenNN::NeuralNetwork
    opennnBridgeController(){}

public:
    /**
     * Singletons should not be cloneable.
     */
    opennnBridgeController(opennnBridgeController &other) = delete; //If another instance is crated , delet it (& = adress)
    /**
     * Singletons should not be assignable.
     */
    void operator=(const opennnBridgeController &) = delete; //disables creating a reference of x in y (& after var = reference)

    // TODO: Think about locking mechanism
    /* opennnBridgeController(data)
     {
         if (instance == nullptr)
         {
         //std::lock_guard<std::mutex> lock(mutex_);
         //if (instance == nullptr)
         //{
             instance = new opennnBridgeController();
         //}
         }
         return instance;
     }*/

    static opennnBridgeController *GetInstance();

    std::shared_ptr<OpenNN::NeuralNetwork> getModelPtr(unsigned long mid){ //shared_ptr is able to point on data edited by number of files/comps.
        return this->_MidNumModel[mid]; //this=opennnBridgeController, go to the selected model's id.
    }

    // Insert new record to the MidNumModel map (new model ptr)
    void setData(std::shared_ptr<OpenNN::NeuralNetwork> modelPtr, unsigned long modelId) {
        this -> _MidNumModel.insert({ modelId, modelPtr }); //Initialize the new data, acording to the selected model id, and its pointer.
    }

    void deleteModel(unsigned long mid){
        this->_MidNumModel.erase(mid); // TODO: Check for memmory leaks
    }
};

/**
 * Static methods should be defined outside the class.
 */
//Initialize pointer to zero so that it can be initialized in first call to getInstance
opennnBridgeController* opennnBridgeController::instance{nullptr}; //First, create an empty object.
//std::mutex opennnBridgeController::mutex_; MUTEX IS USED TO DEAL WITH RACE CONDITIONS.

/**
 * The first time we call GetInstance we will lock the storage location
 *      and then we make sure again that the variable is null and then we
 *      set the value. RU:
 */
opennnBridgeController *opennnBridgeController::GetInstance()
{

    if (instance == nullptr) //if we identify that the object is new (null pointer)
    {
        mutex_.lock(); //use mutex to lock this memory, to prevent race conditions.
        //std::lock_guard<std::mutex> lock(mutex_);
        if (instance == nullptr) //check agin, if the object is still empty, after the previous code line
        {
            instance = new opennnBridgeController(); //then initialize the object with bridgecontroller.
        }
        mutex_.unlock(); //after done, unlock the memory, to enable access to the newly created object.
    }
    return instance;//return the newly created object
}

// TODO: Delete it if not needed
class GetopennnBridgeController {

    opennnBridgeController *s; //create a pointer to a opennnBridgeController type.
public:
    GetopennnBridgeController() { //create a function for the class
        s = s->GetInstance(); //return the bridgeController, with GetInstance (That was defined earlier)
    }

};
