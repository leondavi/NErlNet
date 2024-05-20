-module(workerNN).

-export([controller/2]).

controller(FuncName, {GenWorkerEts, WorkerData}) -> 
  case FuncName of
    init -> init({GenWorkerEts, WorkerData});
    pre_idle -> pre_idle({GenWorkerEts, WorkerData});
    post_idle -> post_idle({GenWorkerEts, WorkerData});
    pre_train -> pre_train({GenWorkerEts, WorkerData});
    post_train -> post_train({GenWorkerEts, WorkerData});
    pre_predict -> pre_predict({GenWorkerEts, WorkerData});
    post_predict -> post_predict({GenWorkerEts, WorkerData});
    start_stream -> start_stream({GenWorkerEts, WorkerData});
    end_stream -> end_stream({GenWorkerEts, WorkerData});
    worker_done -> worker_done({GenWorkerEts, WorkerData});
    update -> update({GenWorkerEts, WorkerData})
  end.

init({_GenWorkerEts, _WorkerData}) -> ok.

pre_idle({_GenWorkerEts, _WorkerData}) -> ok.

post_idle({_GenWorkerEts, _WorkerData}) -> ok.

pre_train({_GenWorkerEts, _WorkerData}) -> ok.

post_train({_GenWorkerEts, _WorkerData}) -> ok.

pre_predict({_GenWorkerEts, _WorkerData}) -> ok.

post_predict({_GenWorkerEts, _WorkerData}) -> ok.

update({_GenWorkerEts, _WorkerData}) -> ok.

start_stream({_GenWorkerEts, _WorkerData}) -> ok.

end_stream({_GenWorkerEts, _WorkerData}) -> ok.

worker_done({_GenWorkerEts, _WorkerData}) -> ok.


