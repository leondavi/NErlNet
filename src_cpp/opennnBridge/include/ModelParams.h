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

	int GetOptimizer() { return _optimizer; };
	unsigned long GetModelId() { return _modelId; };
    double GetLearningRate() { return _learningRate; };
    int GetModelType() { return _modelType; };
	std::shared_ptr<std::vector<int>> GetAcvtivationList() { return _activationList; };
    std::shared_ptr<std::vector<int>> GetLayersSizes() { return _layersSizes; };
    std::shared_ptr<std::vector<int>> GetLayersTypes() { return _layersTypes; };
    

    void setOptimizer(int x){this->_optimizer = x; };
    void setModelId(unsigned long x){this->_modelId = x; };
    void setLearningRate(double x){this->_learningRate = x; };
    void setModelType(int x){this->_modelType = x; };
    void setAcvtivationList(std::shared_ptr<std::vector<int>>  x){this->_activationList = x; };
    void setLayersSizes(std::shared_ptr<std::vector<int>>  x){this->_layersSizes = x; };
    void setLayersTypes(std::shared_ptr<std::vector<int>>  x){this->_layersTypes = x; };


    int _optimizer;
    unsigned long _modelId;
    double _learningRate;
    int _modelType;
    std::shared_ptr<std::vector<int>> _activationList; //shared pointers   std::shared_ptr<std::vector> _activationListPtr; 
    std::shared_ptr<std::vector<int>> _layersSizes; //TODO try to use shared pointers
    std::shared_ptr<std::vector<int>> _layersTypes;
    

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
    
    int GetRows() { return _rows; };
    int GetCol() { return _col; };
    int GetLabels() { return _labels; };
    unsigned long GetOptimizer() { return _mid; };
    ErlNifTid GetTid() { return _tid; };
    ErlNifPid GetPid() { return _pid; };
    std::shared_ptr<std::vector<double>> GetDataLabelMat() { return _data_label_mat; };
    
    void setRows(int x){this->_rows = x; };
    void setCol(int x){this->_col = x; };
    void setLabels(unsigned long x){this->_labels = x; };
    void setTid(ErlNifTid x){this->_tid = x; };
    void setPid(ErlNifPid x){this->_pid = x; };
    void setPid(std::shared_ptr<std::vector<double>> x){this->_data_label_mat = x; };

    int _rows, _col, _labels;
    unsigned long _mid; 
    ErlNifTid _tid;
    ErlNifPid _pid;
    std::shared_ptr<std::vector<double>> _data_label_mat;
private:

};