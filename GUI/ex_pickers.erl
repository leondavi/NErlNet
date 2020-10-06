%%
%% %CopyrightBegin%
%% 
%% Copyright Omayer Gharra 2020. All Rights Reserved.
%% 
%% %CopyrightEnd%

%% This application allow us to read data set from a csv file, send it “for learning mode” to the neural network, predict and show the results with statistics.

-module(ex_pickers).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config
	}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
	FilePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Load Dataset"}]),
    DirPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Result Destination Folder"}]),
    BatchSizeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Batch Size"}]),
					  
    FilePicker = wxFilePickerCtrl:new(Panel, 1, [{path, "/"}]),
    DirPicker = wxDirPickerCtrl:new(Panel, 2, [{path, "/"}]),
	FontPicker = wxTextCtrl:new(Parent, ?wxID_ANY, [{size, {20,20}},
						  {value, "An empty pane"},
						  {style, 0
						   bor ?wxDEFAULT
						   bor ?wxTE_MULTILINE}]),
						   
    wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
    wxDirPickerCtrl:connect(DirPicker, command_dirpicker_changed, []),
	wxFontPickerCtrl:connect(FontPicker, command_fontpicker_changed, []),

    %% Add to sizers
    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],
	wxSizer:add(FilePickerSizer, FilePicker, PickerOptions),
    wxSizer:add(DirPickerSizer, DirPicker, PickerOptions),
	wxSizer:add(BatchSizeSizer, FontPicker, PickerOptions),

    SizerOptions  = [{flag, ?wxEXPAND}],
	wxSizer:add(MainSizer, FilePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, DirPickerSizer, SizerOptions),
	wxSizer:add(MainSizer, BatchSizeSizer, SizerOptions),

	Sizer = wxBoxSizer:new(?wxHORIZONTAL),

    RadioButtonSizer = create_radio_buttons(Panel),

    %% Add to sizers
    wxSizer:add(Sizer, RadioButtonSizer),
    wxSizer:addSpacer(Sizer, 20),
    wxSizer:add(MainSizer, Sizer),
	
	RunButton = wxButton:new(Panel, 100, [{label,"Run"}]),
	wxSizer:add(MainSizer, RunButton),
	
	StatisticsButton = wxButton:new(Panel, 100, [{label,"Show Statistics"}]),
	wxSizer:add(MainSizer, StatisticsButton),
	
    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxFontPicker{font = Font}}, State = #state{}) ->
    demo:format(State#state.config, "Font changed to ~p.\n", [Font]),
    {noreply, State};
handle_event(#wx{event = #wxColourPicker{colour = Colour}}, State = #state{}) ->
    demo:format(State#state.config, "Colour changed to ~p.\n", [Colour]),
    {noreply, State};
handle_event(#wx{event = #wxFileDirPicker{type = command_filepicker_changed,
					  path = Path}},
	     State = #state{}) ->
    demo:format(State#state.config, "Filepicker changed to ~p.\n", [Path]),
    {noreply, State};
handle_event(#wx{event = #wxFileDirPicker{type = command_dirpicker_changed,
					  path = Path}},
	     State = #state{}) ->
    demo:format(State#state.config, "Dirpicker changed to ~p.\n", [Path]),
    {noreply, State};
handle_event(#wx{event = #wxDate{date = Date}},
	     State = #state{}) ->
    demo:format(State#state.config, "Datepicker changed to ~p.\n", [Date]),
    {noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config, "Got Event ~p\n", [Ev]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.


code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_radio_buttons(Panel) ->
    RadioButtonSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
					    [{label, "Mode"}]),

    Buttons =
	[wxRadioButton:new(Panel, ?wxID_ANY, "Train",
			   [{style, ?wxRB_GROUP}]),
	 wxRadioButton:new(Panel, ?wxID_ANY, "Predict", [])],
    Fun =
	fun(Item) ->
		wxRadioButton:connect(Item, command_radiobutton_selected),
		wxSizer:add(RadioButtonSizer, Item)
	end,

    wx:foreach(Fun, Buttons),
    RadioButtonSizer.