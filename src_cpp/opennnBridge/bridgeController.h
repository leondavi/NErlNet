#pragma once 

#include <unordered_map>
#include <mutex>
#include <memory>
#include <stdexcept>
#include "../../opennn/opennn/opennn.h" // adding the api of opennn for creating a model

#ifndef BUILDSCRIPT_PY_BRIDGECONTROLLER_H
#define BUILDSCRIPT_PY_BRIDGECONTROLLER_H

#endif //BUILDSCRIPT_PY_BRIDGECONTROLLER_H

constexpr int BRIDGE_CONTROL_OPENN_NEURAL_NETWORK_TYPE = 0;

std::mutex mutex_;
// Neural network manager singleton
class opennnBridgeController {

private:
    static opennnBridgeController instance;
protected: // Enabling use of the clause only in other classes
    ~opennnBridgeController() {}
    std::unordered_map<unsigned long, std::shared_ptr<opennn::NeuralNetwork>> _MidNumModel; // <Mid,Model struct> Dictionary: choosing a model with  model id key
                                                                                  // SANN::Model to opennn::NeuralNetwork
    std::unordered_map<unsigned long, int> _MidNumModelType;
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

   

    static opennnBridgeController& GetInstance()
    {
        static opennnBridgeController instance;
        return instance;//return the newly created object
    }

    std::shared_ptr<opennn::NeuralNetwork> getModelPtr(unsigned long mid){ //shared_ptr is able to point on data edited by number of files/comps.
        return this->_MidNumModel[mid]; //this=opennnBridgeController, go to the selected model's id.
    }

    int getModelType(unsigned long mid)
    {
        return this->_MidNumModelType[mid];
    }

    // Insert new record to the MidNumModel map (new model ptr)
    void setData(std::shared_ptr<opennn::NeuralNetwork> modelPtr, unsigned long modelId, int modelType = BRIDGE_CONTROL_OPENN_NEURAL_NETWORK_TYPE) {
        if ( !this->_MidNumModel.insert( std::make_pair( modelId, modelPtr ) ).second ) {
            throw("ModelID already exists!");
        } //Initialize the new data, acording to the selected model id, and its pointer.
        
        if ( !this->_MidNumModelType.insert( std::make_pair( modelId, modelType ) ).second ) {
            throw("ModelID already exists!");
        }
    }

    void deleteModel(unsigned long mid){
        if (this->_MidNumModel.find(mid) == m.end())
        {
            throw(std::invalid_argument("model id is not exist in this erlang node!"));
        }
        this->_MidNumModel.erase(mid); // TODO: Check for memmory leaks
        this->_MidNumModelType.erase(mid);
    }
};