-module(utilities_attacher).


client_utilities_attach(ListOfFunctions, Data) -> 
    {ClientPid, ClientEtsRef} = Data, 
    lists:foreach(fun(X, Data) -> X() end,ListOfFunctions).
