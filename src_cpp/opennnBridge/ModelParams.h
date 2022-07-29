/*****************************************************
 * Authors: Evgeni Andrachnic
 * 29/10/2021
 * 
 * @copyright Copyright (c) 2021 Nerlnet
 *****************************************************/ 

#pragma once 

#include <erl_nif.h>
#include <memory>
#include <vector>
#include "../opennn/opennn/opennn.h"


class ModelParams 
{
public:
 	ModelParams() {};
 	ModelParams( unsigned long modelId, int modelType, int scalingMethod,
 	std::shared_ptr<Eigen::Tensor<int,1>> activationList, std::shared_ptr<Eigen::Tensor<int,1>> layersSizes,
    std::shared_ptr<Eigen::Tensor<int,1>> layersTypes);


	
	unsigned long GetModelId() { return _modelId; };
    int GetModelType() { return _modelType; };
    int GetScalingMethod() { return _scalingMethod; };
	std::shared_ptr<Eigen::Tensor<int,1>> GetAcvtivationList() { return _activationList; };
    std::shared_ptr<Eigen::Tensor<int,1>> GetLayersSizes() { return _layersSizes; };
    std::shared_ptr<Eigen::Tensor<int,1>> GetLayersTypes() { return _layersTypes; };

    //int GetOptimizer() { return _optimizer; };
    //double GetLearningRate() { return _learningRate; };
    

    
    void setModelId(unsigned long x){this->_modelId = x; };
    void setModelType(int x){this->_modelType = x; };
    void setScalingMethod(int x){this->_scalingMethod = x; };
    void setAcvtivationList(std::shared_ptr<Eigen::Tensor<int,1>>  x){this->_activationList = x; };
    void setLayersSizes(std::shared_ptr<Eigen::Tensor<int,1>> x){this->_layersSizes = x; };
    void setLayersTypes(std::shared_ptr<Eigen::Tensor<int,1>> x){this->_layersTypes = x; };

    //void setOptimizer(int x){this->_optimizer = x; };
    //void setLearningRate(double x){this->_learningRate = x; };


    
    unsigned long _modelId;
    int _modelType;
    int _scalingMethod;
    std::shared_ptr<Eigen::Tensor<int,1>> _activationList; //shared pointers   std::shared_ptr<std::vector> _activationListPtr; 
    std::shared_ptr<Eigen::Tensor<int,1>> _layersSizes; //TODO try to use shared pointers
    std::shared_ptr<Eigen::Tensor<int,1>> _layersTypes;
    
    //double _learningRate;
    //int _optimizer;
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







