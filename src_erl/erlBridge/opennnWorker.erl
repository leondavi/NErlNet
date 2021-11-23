-module(opennnWorker).
-include("") % include opennnWorkerDefinitions.hrl
%% The opennn worker contains NIFPP implemntations 
-export([initialize/x,train/x,predict/x,getWorkerId/0]).
-on_load() % load so 

getWrokerId(pid) -> workerId. 

initialize() -> ok %call to function of nif.