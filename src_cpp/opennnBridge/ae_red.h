#pragma once

#define PARAM_K_DEFAULT 0.5f
#define ALPHA_DEFAULT 0.3f

#include "eigenTensorTypes.h"


namespace nerlnet
{

class AeRed
{
    
    public:
    
    AeRed(float k = PARAM_K_DEFAULT , float alpha = ALPHA_DEFAULT);
    ~AeRed();

    fTensor2DPtr update_batch(fTensor2DPtr loss_values);
    float update_sample(float loss_value);
    float _k;
    float _alpha;
    float _threshold;
    float _ema;
    float _emad;
    float _ema_event;
    float _ema_normal;
    float _prev_ema;
    float _prev_emad;

};

} // namespace nerlnet
