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
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []}
        
        ]}
    ]),
    {ok, _} = cowboy:start_clear(gui_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    GUI = wx:new(),
    StartFrame = mainScreen:new(GUI, ""),
    mainScreen:show(StartFrame),

    nerlGUI_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
