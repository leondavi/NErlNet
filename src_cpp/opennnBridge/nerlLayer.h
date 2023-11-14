#pragma once

#include "eigenTensorTypes.h"
#include "worker_definitions_ag.h"

class NerlLayer
{
    public:
    NerlLayer(int layer_type, iTensor1D &layer_size, int layer_functionality);
    ~NerlLayer();

    private:
    int _layer_type;
    int _layer_size;
    int _layer_functionality;
};