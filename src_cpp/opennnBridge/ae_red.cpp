#include "ae_red.h"

namespace nerlnet
{
    
AeRed::AeRed(const std::string &_model_args_str, float k, float alpha) 
{
    getModelArgsParsed(_model_args_str, _model_args_parsed);
    _k = _model_args_parsed.k;
    _alpha = _model_args_parsed.alpha;
    _ema = 0;
    _emad = 1;
    _ema_event = 0;
    _ema_normal = 0;
    _prev_ema = 0;
    _prev_emad = 0;
}

void AeRed::getModelArgsParsed(const std::string &_model_args_str, ModelArgsParsed_t &model_args_parsed){
    std::string k_str = "k=";
    std::string alpha_str = "alpha=";
    std::string use_ema_only_str = "use_ema_only=";
    std::size_t found_k = _model_args_str.find(k_str);
    std::size_t found_alpha = _model_args_str.find(alpha_str);
    std::size_t found_use_ema_only = _model_args_str.find(use_ema_only_str);
    
    try {
        if (found_k != std::string::npos) {
            std::string k_value_str = _model_args_str.substr(found_k + k_str.length());
            model_args_parsed.k = std::stof(k_value_str);
        } else {
            model_args_parsed.k = PARAM_K_DEFAULT;
        }
        
        if (found_alpha != std::string::npos) {
            std::string alpha_value_str = _model_args_str.substr(found_alpha + alpha_str.length());
            model_args_parsed.alpha = std::stof(alpha_value_str);
        } else {
            model_args_parsed.alpha = ALPHA_DEFAULT;
        }
        
        if (found_use_ema_only != std::string::npos) {
            std::string use_ema_only_value_str = _model_args_str.substr(found_use_ema_only + use_ema_only_str.length());
            model_args_parsed.use_ema_only = std::stoi(use_ema_only_value_str);
        } else {
            model_args_parsed.use_ema_only = 0;
        }
    } catch (const std::invalid_argument &e) {
        std::cerr << "Invalid argument: " << e.what() << std::endl;
        // Handle error or set default values
        model_args_parsed.k = PARAM_K_DEFAULT;
        model_args_parsed.alpha = ALPHA_DEFAULT;
        model_args_parsed.use_ema_only = 0;
    } catch (const std::out_of_range &e) {
        std::cerr << "Out of range: " << e.what() << std::endl;
        // Handle error or set default values
        model_args_parsed.k = PARAM_K_DEFAULT;
        model_args_parsed.alpha = ALPHA_DEFAULT;
        model_args_parsed.use_ema_only = 0;
    }
}

AeRed::~AeRed(){}

fTensor2DPtr AeRed::update_batch(fTensor2DPtr loss_values)
{
    fTensor2DPtr result = std::make_shared<fTensor2D>(loss_values->dimension(0), loss_values->dimension(1));
    for(int i = 0; i < (*loss_values).dimension(0); i++)
    {
        (*result)(i, 0) = update_sample((*loss_values)(i, 0), i);
    }
    return result;
}

float AeRed::update_sample(float loss_value, int index)
{
    if(_model_args_parsed.use_ema_only){
        return update_sample_ema(loss_value, index);
    }
    else{
        return update_sample_red(loss_value, index);
    }
}

float AeRed::update_sample_red(float loss_value, int index){
    if (_ema != 0) {
        _ema = _alpha * loss_value + (1 - _alpha) * _prev_ema;
    }
    else {
        _ema = loss_value;
    }
    _prev_ema = _ema;
    if (_emad != 1) {
        _emad = _alpha * abs(loss_value - _ema) + (1 - _alpha) * _prev_emad;
    }
    else {
        _emad = loss_value;
    }
    _prev_emad = _emad;
    if(_ema + _k * _emad < loss_value){
        _ema_event = loss_value;
    }
    else{
        _ema_normal = loss_value;
    }
    _threshold = (_ema_event + _ema_normal) / 2; // New Threshold

    if(loss_value > _threshold) 
        return 1.f;
    else 
        return 0.f;
}

float AeRed::update_sample_ema(float loss_value, int index)
{
    if (_ema != 0) {
        _ema = _alpha * loss_value + (1 - _alpha) * _prev_ema;
    }
    else {
        _ema = loss_value;
    }
    _prev_ema = _ema;
    _threshold = _ema / 2;
    if(loss_value > _threshold) 
        return 1.f;
    else 
        return 0.f;
}


} // namespace nerlnet