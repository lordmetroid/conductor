-module(conductor_execute).

-behavior(gen_server).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	start_link/0,
	get_program/1,
	get_model/1,
	get_view/1,
	get_controller/1
]).

%% ----------------------------------------------------------------------------
% @spec
% @doc Compile and cache all application components available
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	%% Compile and cache components
	{ok, {
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(program_path), "*.erl"
		]))),
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(model_path), "*.erl"
		]))),
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(view_path), "*.*"
		]))),
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(controller_path), "*.erl"
		])))
	}}.

handle_call({get_program, ProgramName}, _From, Cache) ->
	{Programs,_,_,_} = Cache,
	%% TODO: Check if file is updated and update cache
	proplists:get_value(ProgramName, Programs);

handle_call({get_model, ModelName}, _From, Cache) ->
	{_,Models,_,_} = Cache,
	%% TODO: Check if file is updated and update cache
	proplists:get_value(ModelName, Models);
	
handle_call({get_view, ViewName}, _From, Cache) ->
	{_,_,Views,_} = Cache,
	%% TODO: Check if file is updated and update cache
	proplists:get_value(View, Views);

handle_call({get_controller, ControllerName}, _From, Cache) ->
	{_,_,_,Controllers} = Cache,
	%% TODO: Check if file is updated and update cache
	proplists:get_value(ControllerName, Controllers);

handle_call(_Event, _From, State) ->
	{stop, State}.

handle_cast(_Event, State) ->
	{stop, State}.

handle_info(_Information, State) ->
	{stop, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% ----------------------------------------------------------------------------
% @spec start() ->
% @doc Start the web application file cache manager
%% ----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
% @spec get_...() ->
% @doc Get the cached web application file
%% ----------------------------------------------------------------------------
get_program(ProgramName) ->
	gen_server:call(?MODULE, {get_program, ProgramName}).
get_model(ModelName) ->
	get_server:call(?MODULE, {get_model, ModelName}).
get_view(ViewName) ->
	get_server:call(?MODULE, {get_view, ViewName}).
get_controller(ControllerName) ->
	get_controller:call(?MODULE, {get_controller, ControllerName}).
