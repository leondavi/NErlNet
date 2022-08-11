-include_lib("wx/include/wx.hrl").

-record(state, {parent, frame}).

-define(PADDING_W, 10).
-define(PADDING_H, 20).
-define(BUTTON_W, 200).
-define(BUTTON_H, 200).
-define(BUTTON_SIZE(Mult), {size, {round(?BUTTON_W*Mult), round(?BUTTON_H*Mult)}}).
-define(BUTTON_LOC(Row, Col), 
    {pos, {round((Col+1) * ?PADDING_H + Col * ?BUTTON_H), round((Row+1) * ?PADDING_W + Row * ?BUTTON_W)}}).