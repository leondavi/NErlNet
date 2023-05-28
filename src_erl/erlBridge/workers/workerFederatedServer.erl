-module(workerFederatedServer).

-export([controller/2]).

controller(FuncName, Data) -> 
  case FuncName of
    init -> init(Data);
    pre_train -> pre_train(Data);
    post_train -> post_train(Data);
    pre_predict -> pre_predict(Data);
    post_predict -> post_predict(Data)
  end.

%% handshake with workers / server (DONE IN GENERIC)
init(Data) -> Data.

%% every countLimit batches, get updated model (NOTHING FOR SERVER)
pre_train(Data) -> Data.

%% every countLimit batches, send updated weights
post_train(Data) -> Data.

%% nothing?
pre_predict(Data) -> Data.

%% nothing?
post_predict(Data) -> Data.

