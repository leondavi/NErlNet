%%%-------------------------------------------------------------------
%% @doc nerlGUI public API
%% @end
%%%-------------------------------------------------------------------

-module(nerlGUI_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:start(sasl),
    application:start(ranch),
    application:start(inets),
    
    GUI = wx:new(),
    StartFrame = mainScreen:new(GUI, ""),
    mainScreen:show(StartFrame),

    ScreenPID = wx_object:get_pid(StartFrame),
    io:format("Screen PID=~p~n", [ScreenPID]),
    ScreenPID ! {addInfo, "init GUI"},
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, [StartFrame]}
        
        ]}
    ]),
    {ok, _} = cowboy:start_clear(gui_listener,
        [{port, 8096}],
        #{env => #{dispatch => Dispatch}}
    ),

    nerlGUI_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
