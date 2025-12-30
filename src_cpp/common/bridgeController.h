#pragma once 

#include <unordered_map>
#include <memory>
#include <stdexcept>

#include <nifpp.h>

#include "nerlWorker.h" // adding the api of opennn for creating a model



// Neural network manager singleton
namespace nerlnet{
class BridgeController {

private:
    static BridgeController instance;
protected: // Enabling use of the clause only in other classes
    ~BridgeController() {}
    std::unordered_map<unsigned long, std::shared_ptr<NerlWorker>> _MidNumModel; // <Mid,Model struct> Dictionary: choosing a model with  model id key
                                                                                  // SANN::Model to opennn::NeuralNetwork
    BridgeController(){}
public:
    /**
     * Singletons should not be cloneable.
     */
    BridgeController(BridgeController &other) = delete; //If another instance is crated , delet it (& = adress)
    /**
     * Singletons should not be assignable.
     */
    void operator=(const BridgeController &) = delete; //disables creating a reference of x in y (& after var = reference)

    static BridgeController& GetInstance()
    {
        static BridgeController instance;
        return instance;//return the newly created object
    }

    std::shared_ptr<NerlWorker> getModelPtr(unsigned long mid){ //shared_ptr is able to point on data edited by number of files/comps.
        return this->_MidNumModel[mid]; //this=opennnBridgeController, go to the selected model's id.
    }

    void get_models_ids_list(std::vector<unsigned long> &output_vec)
    {
        std::unordered_map<unsigned long, std::shared_ptr<NerlWorker>>::iterator it;
        for(it = this->_MidNumModel.begin(); it != this->_MidNumModel.end(); ++it)
        {
            output_vec.push_back(it->first);
        }
    }

    // Insert new record to the MidNumModel map (new model ptr)
    void setData(std::shared_ptr<NerlWorker> modelPtr, unsigned long modelId ) {
        if ( !this->_MidNumModel.insert( std::make_pair( modelId, modelPtr ) ).second ) {
            throw("ModelID already exists!");
        } //Initialize the new data, acording to the selected model id, and its pointer.
    }

    void deleteModel(unsigned long mid){
        if (this->_MidNumModel.find(mid) == _MidNumModel.end())
        {
            throw std::runtime_error("unexpected bridge controller behavior - maps are not identical!");
        }
        this->_MidNumModel.erase(mid); // TODO: Check for memmory leaks
    }
};
} // namespace nerlnet


static ERL_NIF_TERM get_active_models_ids_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    using namespace nerlnet;
    std::vector<unsigned long> mids_list;
    BridgeController& onnBrCtrl = BridgeController::GetInstance();
    onnBrCtrl.get_models_ids_list(mids_list);

    return nifpp::make(env, mids_list);
}