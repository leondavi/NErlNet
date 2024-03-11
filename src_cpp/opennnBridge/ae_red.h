#pragma once
#define PARAM_K_DEFAULT 1.2f
#define ALPHA_DEFAULT 0.3f
#include "eigenTensorTypes.h"


using namespace nerlnet;

class AeRed
{
    private:
    float _k;
    float _alpha;
    float _threshold;
    float _ema = 0;
    float _emad = 1;
    float _ema_event = 0;
    float _ema_normal = 0;
    float _prev_ema = 0;
    float _prev_emad = 0;
    
    public:
    AeRed(float k = PARAM_K_DEFAULT , float alpha = ALPHA_DEFAULT);
    ~AeRed();

    fTensor1DPtr update_batch(fTensor1D loss_values);
    float update_sample(float loss_value);

};
