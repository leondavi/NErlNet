#include "ae_red.h"


AeRed::AeRed(float k , float alpha)
{
    _k = k;
    _alpha = alpha;
}

fTensor1DPtr AeRed::update_batch(fTensor1D loss_values)
{
    fTensor1DPtr result = std::make_shared<fTensor1D>(loss_values.size());
    for(int i = 0; i < loss_values.size() - 1; i++)
    {
        cout << "GOT HERE" << i << endl;
        cout << "loss["<<i<<"] = " << loss_values(i) << endl;
        float val = update_sample(loss_values(i));
        result->data()[i] = val;
    }
    return result;
}

float AeRed::update_sample(float loss_value){
    cout << "GOT TO UPDATE SAMPLE WITH LOSS VALUE: " << loss_value << endl; // ? Why it raises a segmentation fault after this line
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
    else return 0;
}
