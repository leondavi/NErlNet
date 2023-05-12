-define (LIST_GROUP_NERLTENSOR_TYPE, [erl_float, erl_int]).
-define (LIST_BINARY_INT_NERLTENSOR_TYPE, [int32, int16]).
-define (LIST_BINARY_FLOAT_NERLTENSOR_TYPE, [float,double]).

% bytes_str: list of chars in Erlang and std::string in NIF
% float32/double: list of floats in Erlang
% int32: list of integers in Erlang

-define(nerlTensorAsList(_nerlTensor), [#_nerlTensor.x,#_nerlTensor.y,#_nerlTensor.z]++#_nerlTensor.data).

-define(NERL_TYPES,
        [{Key, erl_int} || Key <- ?LIST_BINARY_INT_NERLTENSOR_TYPE] ++ 
        [{Key, erl_float} || Key <- ?LIST_BINARY_FLOAT_NERLTENSOR_TYPE]).