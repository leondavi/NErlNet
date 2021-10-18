#pragma once 

#include <memory>
#include <vector>

class ModelParams 
{
public:
 	ModelParams() {};
 	ModelParams(int optimizer, unsigned long modelId, double learningRate,int modelType, 
 	std::shared_ptr<std::vector<int>> activationList, std::shared_ptr<std::vector<int>> layersSizes);

	int GetOptimizer() { return _optimizer; }
	
	std::shared_ptr<std::vector<int>> GetAcvtivationList() { return _activationList; };
    std::shared_ptr<std::vector<int>> GetLayersSizes() { return _layersSizes; };
    int GetModelType() { return _modelType; };

    int _optimizer;
    unsigned long _modelId;
    double _learningRate;
    std::shared_ptr<std::vector<int>> _activationList; //shared pointers   std::shared_ptr<std::vector> _activationListPtr; 
    std::shared_ptr<std::vector<int>> _layersSizes; //TODO try to use shared pointers
    int _modelType;

private:


};
