-include_lib("wx/include/wx.hrl").

-record(state, {parent, frame}).

-define(MAINSERVER_URL, "http://127.0.0.1:8080").


-define(PADDING_W, 10).
-define(PADDING_H, 20).
-define(TILE_W, 200).
-define(TILE_H, 200).
-define(BUTTON_SIZE(Mult), {size, {round(?TILE_W*Mult), round(?TILE_H*Mult)}}).
-define(BUTTON_LOC(Row, Col), 
    {pos, {round((Col+1) * ?PADDING_H + Col * ?TILE_H), round((Row+1) * ?PADDING_W + Row * ?TILE_W)}}).