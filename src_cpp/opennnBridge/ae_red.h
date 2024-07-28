#pragma once

#define PARAM_K_DEFAULT 1.7f
#define ALPHA_DEFAULT 0.4f

#include "eigenTensorTypes.h"

namespace nerlnet
{

class AeRed
{
    typedef struct ModelArgsParsed {
        float k;
        float alpha;
        bool use_ema_only;
    } ModelArgsParsed_t;
    
    public:
    
    AeRed(const std::string &_model_args_str, float k = PARAM_K_DEFAULT , float alpha = ALPHA_DEFAULT);
    ~AeRed();

    fTensor2DPtr update_batch(fTensor2DPtr loss_values);
    float update_sample(float loss_value, int index);
    float update_sample_red(float loss_value, int index);
    float update_sample_ema(float loss_value, int index);
    void getModelArgsParsed(const std::string &_model_args_str, ModelArgsParsed_t &model_args_parsed);

    float _k;
    float _alpha;
    float _threshold;
    float _ema;
    float _emad;
    float _ema_event;
    float _ema_normal;
    float _prev_ema;
    float _prev_emad;
    ModelArgsParsed_t _model_args_parsed;
};

} // namespace nerlnet
