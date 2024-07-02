#include "ae_red.h"

namespace nerlnet
{
    
AeRed::AeRed(float k, float alpha) // TODO Add ModelArgs and parse them here
{
    _k = k;
    _alpha = alpha;
    _ema = 0;
    _emad = 1;
    _ema_event = 0;
    _ema_normal = 0;
    _prev_ema = 0;
    _prev_emad = 0;
}

AeRed::~AeRed()
{

}

fTensor2DPtr AeRed::update_batch(fTensor2DPtr loss_values)
{
    fTensor2DPtr result = std::make_shared<fTensor2D>(loss_values->dimension(0), loss_values->dimension(1));
    for(int i = 0; i < (*loss_values).dimension(0); i++)
    {
        float val = update_sample((*loss_values)(i, 0));
        if ((*loss_values)(i) == val) (*result)(i, 0) = 1;
        else (*result)(i, 0) = 0;
    }
    return result;
}

float AeRed::update_sample(float loss_value){
    _ema = _alpha * loss_value + (1 - _alpha) * _prev_ema;
    _prev_ema = _ema;
    _emad = _alpha * abs(loss_value - _ema) + (1 - _alpha) * _prev_emad;
    _prev_emad = _emad;
    if(_ema + _k * _emad < loss_value){
        _ema_event = loss_value;
    }
    else{
        _ema_normal = loss_value;
    }
    _threshold = (_ema_event + _ema_normal) / 2; // New Threshold

    if(loss_value > _threshold) return loss_value;
    else return -loss_value;
}

} // namespace nerlnet