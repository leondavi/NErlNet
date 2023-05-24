-module(workerNN).


controller(FuncName, Data) -> 
  case FuncName of
    init -> init(Data);
    pre_train -> pre_train(Data);
    post_train -> post_train(Data);
    pre_predict -> pre_predict(Data);
    post_predict -> post_predict(Data)
  end.

init() -> ok.

pre_train() -> ok.

post_train() -> ok.

pre_predic() -> ok.

post_predic() -> ok.

