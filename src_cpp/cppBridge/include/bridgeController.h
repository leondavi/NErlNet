//
// Created by ziv on 17/04/2021.
//

#include <unordered_map>
#include <mutex>
#include <memory>

#ifndef BUILDSCRIPT_PY_BRIDGECONTROLLER_H
#define BUILDSCRIPT_PY_BRIDGECONTROLLER_H

#endif //BUILDSCRIPT_PY_BRIDGECONTROLLER_H

std::mutex mutex_;
// Neural network manager singleton
class cppBridgeController {
private:
    static cppBridgeController *instance;
protected:
    ~cppBridgeController() {}
    std::unordered_map<unsigned long, std::shared_ptr<SANN::Model>> _MidNumModel; // <Mid,Model struct>

    cppBridgeController(){}

public:
    /**
     * Singletons should not be cloneable.
     */
    cppBridgeController(cppBridgeController &other) = delete;
    /**
     * Singletons should not be assignable.
     */
    void operator=(const cppBridgeController &) = delete;

    // TODO: Think about locking mechanism
    /* cppBridgeController(data)
     {
         if (instance == nullptr)
         {
         //std::lock_guard<std::mutex> lock(mutex_);
         //if (instance == nullptr)
         //{
             instance = new cppBridgeController();
         //}
         }
         return instance;
     }*/

    static cppBridgeController *GetInstance();

    std::shared_ptr<SANN::Model> getModelPtr(unsigned long mid){
        return this->_MidNumModel[mid];
    }

    // Insert new record to the MidNumModel map (new model ptr)
    void setData(std::shared_ptr<SANN::Model> modelPtr, unsigned long modelId) {
        this -> _MidNumModel.insert({ modelId, modelPtr });
    }

    void deleteModel(unsigned long mid){
        this->_MidNumModel.erase(mid); // TODO: Check for memmory leaks
    }
};

/**
 * Static methods should be defined outside the class.
 */
//Initialize pointer to zero so that it can be initialized in first call to getInstance
cppBridgeController* cppBridgeController::instance{nullptr};
//std::mutex cppBridgeController::mutex_;

/**
 * The first time we call GetInstance we will lock the storage location
 *      and then we make sure again that the variable is null and then we
 *      set the value. RU:
 */
cppBridgeController *cppBridgeController::GetInstance()
{

    if (instance == nullptr)
    {
        mutex_.lock();
        //std::lock_guard<std::mutex> lock(mutex_);
        if (instance == nullptr)
        {
            instance = new cppBridgeController();
        }
        mutex_.unlock();
    }
    return instance;
}

// TODO: Delete it if not needed
class GetcppBridgeController {

    cppBridgeController *s;
public:
    GetcppBridgeController() {
        s = s->GetInstance();
    }

};