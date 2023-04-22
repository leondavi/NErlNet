-type nerlTensorType() :: erl_float | erl_int | float32 | double | int32.
-define (ERL_GROUP_NERLTENSOR_TYPE, [erl_float, erl_int]).
-define (CHAR_BYTES_GROUP_NERLTENSOR_TYPE, [float32,double,int32]).

% bytes_str: list of chars in Erlang and std::string in NIF
% float32/double: list of floats in Erlang
% int32: list of integers in Erlang

-define(nerlTensorAsList(_nerlTensor), [#_nerlTensor.x,#_nerlTensor.y,#_nerlTensor.z]++#_nerlTensor.data).

