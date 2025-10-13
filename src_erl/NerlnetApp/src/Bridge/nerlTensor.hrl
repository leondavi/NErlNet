-define (LIST_GROUP_NERLTENSOR_TYPE, [erl_float, erl_int]).
-define (LIST_BINARY_INT_NERLTENSOR_TYPE, [int32, int16]). % add uint8
-define (LIST_BINARY_FLOAT_NERLTENSOR_TYPE, [float,double]).

-define(NUMOF_DIMS,3).
% nif return timeouts
-define(PREDICT_TIMEOUT,600000). % 600 seconds limit for prediction results
-define(TRAIN_TIMEOUT,600000). % 600 seconds limit for training results

%% nerlNIF defines
-define(NERLNET_LIB,"libnerlnet_onn").
-define(NERLNET_PATH,"/usr/local/lib/nerlnet-lib/NErlNet").
-define(BUILD_TYPE_DEBUG,"debug").
-define(BUILD_TYPE_RELEASE,"/build/release").


% -define(LOG_HEADER, atom_to_list(?MODULE) ++ "/" ++ atom_to_list(?FUNCTION_NAME) ++ "@" ++ integer_to_list(?LINE) ++ ": " ).

% bytes_str: list of chars in Erlang and std::string in NIF
% float32/double: list of floats in Erlang
% int32: list of integers in Erlang

-define(nerlTensorAsList(_nerlTensor), [#_nerlTensor.x,#_nerlTensor.y,#_nerlTensor.z]++#_nerlTensor.data).

-define(NERL_TYPES,
        [{Key, erl_int} || Key <- ?LIST_BINARY_INT_NERLTENSOR_TYPE] ++ 
        [{Key, erl_float} || Key <- ?LIST_BINARY_FLOAT_NERLTENSOR_TYPE]).