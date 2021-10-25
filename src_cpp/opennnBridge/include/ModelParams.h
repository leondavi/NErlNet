#pragma once 

#include <erl_nif.h>
#include <memory>
#include <vector>

class ModelParams 
{
public:
 	ModelParams() {};
 	ModelParams(int optimizer, unsigned long modelId, double learningRate,int modelType, 
 	std::shared_ptr<std::vector<int>> activationList, std::shared_ptr<std::vector<int>> layersSizes,
    std::shared_ptr<std::vector<int>> layersTypes);

	int GetOptimizer() { return _optimizer; }
	
	std::shared_ptr<std::vector<int>> GetAcvtivationList() { return _activationList; };
    std::shared_ptr<std::vector<int>> GetLayersSizes() { return _layersSizes; };
    std::shared_ptr<std::vector<int>> GetLayersTypes() { return _layersTypes; };
    int GetModelType() { return _modelType; };


    int _optimizer;
    unsigned long _modelId;
    double _learningRate;
    std::shared_ptr<std::vector<int>> _activationList; //shared pointers   std::shared_ptr<std::vector> _activationListPtr; 
    std::shared_ptr<std::vector<int>> _layersSizes; //TODO try to use shared pointers
    std::shared_ptr<std::vector<int>> _layersTypes;
    int _modelType;

private:

};

// Train mode parameters struct
// rows, col - Represents the rows and columns of the data matrix
// labels - Represents the label portion of the label matrix from the data_label matrix
// mid - model id number
// data_Label_mat - list in erlang. Represents tha data and the label matrix (last columns) together
// tid - unique thread identification
// pid - unique erlang process identification



class TrainParams {
public:

    TrainParams() {};
    TrainParams(int rows, int col, int labels, unsigned long mid,std::shared_ptr<std::vector<double>> data_label_mat, 
			  ErlNifTid tid, ErlNifPid pid);
    

    std::shared_ptr<std::vector<double>> GetDataLabelMat() { return _data_label_mat; };

    int _rows, _col, _labels;
    unsigned long _mid;
    std::shared_ptr<std::vector<double>> _data_label_mat;
    ErlNifTid _tid;
    ErlNifPid _pid;

private:

};