-module(conductor_cache).

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
	%% Get component paths
	ProgramPath = conductor_settings:get(program_path),
	ModelPath = conductor_settings:get(model_path),
	ViewPath = conductor_settings:get(view_path),
	ControllerPath = conductor_settings:get(controller_path),

	%% Compile available components
	{ok, {
		conductor_compiler:make_programs(ProgramPath), 
		conductor_compiler:make_models(ModelPath), 
		conductor_compiler:make_views(ViewPath), 
		conductor_compiler:make_controllers(ControllerPath)
	}}.

handle_call({get_program, Program}, _From, Cache) ->
	{Programs,_,_,_} = Cache,
	%% TODO: Check if file is updated and update cache
	proplists:get_value(Program, Programs);

handle_call({get_model, Model}, _From, Cache) ->
	{_,Models,_,_} = Cache,
	proplists:get_value(Model, Models);
	
handle_call({get_view, View}, _From, Cache) ->
	{_,_,Views,_} = Cache,
	proplists:get_value(View, Views);

handle_call({get_controller, Controller}, _From, Cache) ->
	{_,_,_,Controllers} = Cache,
	proplists:get_value(Controller, Controllers);

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
% @doc Start the settings manager 
%% ----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_program(Program) ->
	gen_server:call(?MODULE, {get_program, Program}).

get_model(Model) ->
	get_server:call(?MODULE, {get_model, Model]).
	
get_view(View) ->
	get_server:call(?MODULE, {get_view, View}).

get_controller(Controller) ->
	get_controller:call(?MODULE, {get_controller, Controller}).
