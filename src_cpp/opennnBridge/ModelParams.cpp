#include "include/ModelParams.h"

ModelParams::ModelParams(int optimizer, unsigned long modelId, double learningRate,int modelType, 
			  std::shared_ptr<std::vector<int>> activationList, std::shared_ptr<std::vector<int>> layersSizes) : 
_optimizer(optimizer),
_modelId(modelId),
_learningRate(learningRate),
_activationList(activationList),            
_layersSizes(layersSizes)
{

};
