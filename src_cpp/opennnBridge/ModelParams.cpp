#include "include/ModelParams.h"
#include "include/CustumNN.h"

/*
  model parameters class:
  modelId - "  " in erlang . represents the model Id in singelton.
  modeltype - "   " in erlang . represents the model type .
                 1 - Approximation , 2 - Classification , 3 - Forecasting.

  layers_sizes - list in erlang. represents the layers sizes of the neural network.
  layers_sizes - list in erlang. represents the layers types of the neural network.
                 1 - Scaling , 2 - Convolutional , 3 - Perceptron , 4 - Pooling , 5 - Probabilistic , 6 - LongShortTermMemory ,
				 7 - Recurrent , 8 - Unscaling , 9 - Bounding. 

  activation functions - 
             			 1 - Threshold , 2 - SymmetricThreshold , 3 - Logistic , 4 - HyperbolicTangent ,
                         5 - Linear , 6 - RectifiedLinear , 7 - ExponentialLinear , 8 - ScaledExponentialLinear ,
                         9 - SoftPlus , 10 - SoftSign , 11 - HardSigmoid.

  	layers_sizes - activation functions: 


  Learning_rate - number (0-1). Usually 1/number_of_samples
  Train_set_size - percentage number. Usually 70%-80%. Represents the portion of the data that we train
  Activation_list (optional) - list
  


*/

ModelParams::ModelParams(unsigned long modelId, int modelType, int scalingMethod,
			  std::shared_ptr<Eigen::Tensor<int,1>> activationList, std::shared_ptr<Eigen::Tensor<int,1>> layersSizes,
			  std::shared_ptr<Eigen::Tensor<int,1>> layersTypes) : 

//_optimizer(optimizer),
//_learningRate(learningRate),

_modelId(modelId),
_modelType(modelType),
_scalingMethod(scalingMethod),
_activationList(activationList),            
_layersSizes(layersSizes),
_layersTypes(layersTypes)


{

};



TrainParams::TrainParams(int rows, int col, int labels, unsigned long mid, std::shared_ptr<std::vector<double>> data_label_mat, 
			  ErlNifTid tid, ErlNifPid pid) :

_rows(rows),
_col(col),
_labels(labels),
_mid(mid),
_data_label_mat(data_label_mat),
_tid(tid),
_pid(pid)

{

};



/*
CustumNN::CustumNN ():  //(OpenNN::NeuralNetwork neuralNetwork, Eigen::Tensor<int,1> layersSizes,
                    //Eigen::Tensor<int,1> layersTypes ,  Eigen::Tensor<int,1> activationFunctions) :
 
//_neuralNetwork(neuralNetwork),
//_layersSizes(layersSizes),
//_layersTypes(layersTypes),
//_activationFunctions(activationFunctions)

{

};    
*/