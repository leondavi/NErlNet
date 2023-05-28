-module(workerNN).

-export([controller/2]).

controller(FuncName, Data) -> 
  case FuncName of
    init -> init(Data);
    pre_train -> pre_train(Data);
    post_train -> post_train(Data);
    pre_predict -> pre_predict(Data);
    post_predict -> post_predict(Data)
  end.

init(Data) -> Data.

pre_train(Data) -> Data.

post_train(Data) -> Data.

pre_predict(Data) -> Data.

post_predict(Data) -> Data.

