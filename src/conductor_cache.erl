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
% @doc Compile and cache all web application files available
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	%% Compile and cache modules
	{ok, {
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(program_root), "*.erl"
		]))),
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(model_root), "*.erl"
		]))),
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(view_root), "*.*"
		]))),
		conductor_compiler:make(filelib:wildcard(filename:join([
			conductor_settings:get(controller_root), "*.erl"
		])))
	}}.

handle_call({get_program, ProgramFile}, _From, Caches) ->
	{Programs, M,V,C} = Caches,
	
	ProgramRoot = conductor_settings:get(program_root),
	ProgramPath = filename:join([ProgramRoot, ProgramFile]),

	case update_cache(ProgramFile, ProgramPath, Programs) of
		false ->
			%% Program does not exist
			{reply, false, Caches};
		{Program, NewPrograms} ->
			%% Program found and cache has been updated
			{reply, Program, {NewPrograms, M,V,C}}
	end;

handle_call({get_model, ModelFile}, _From, Caches) ->
	{P,Models,V,C} = Caches,

	ModelRoot = conductor_settings:get(model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),

	case update_cache(ModelFile, ModelPath, Models) of
		false ->
			%% Model does not exist
			{reply, false, Caches};
		{Model, NewModels} ->
			%% Model found and cache has been updated
			{reply, Model, {P,NewModels,V,C}
	end;

handle_call({get_view, ViewFile}, _From, Caches) ->
	{P,M,Views,C} = Cache,

	ViewRoot = conductor_settings:get(view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),

	case update_cache(ViewFile, ViewPath, Views) of
		false ->
			%% View does not exist
			{reply, false, Caches};
		{View, NewViews} ->
			%% View found and cache has been updated
			{reply, View, {P,M,NewViews,C}
	end;

handle_call({get_controller, ControllerFile}, _From, Caches) ->
	{P,M,V,Controllers} = Cache,

	ControllerRoot = conductor_settings:get(controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),

	case update_cache(ControllerFile, ControllerPath, Controllers) of
		false ->
			%% Controller does not exist
			{reply, false, Caches};
		{Controller, NewControllers}
			%% Controller found and cache has been updated
			{reply, Controller, {P,M,V,NewControllers}
	end;

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

update_cache(File, FilePath, Cache)
	case lists:keyfind(File,1, Cache) of
		false ->
			case filelib:last_modified(FilePath) of
				0 ->
					%% File does exists in cache nor in filesystem
					false;
				Date ->
					%% Cache new file from filesystem
					NewModule = conductor_compiler:make(ProgramPath),
					{NewModule, [NewModule, Cache]}
			end;
		{File, {Module, Date}} ->
			case filelib:last_modified(FilePath) of
				0 ->
					%% Remove deleted file from cache
					NewCache = lists:keydelete(File, 1, Cache),
					false;
				Date ->
					%% Cache is up to date
					{Module, Cache};
				NewDate ->
					%% Update cache with new file
					NewModule = conductor_compiler:make(FilePath),
					NewEntry = {NewModule, NewDate},
					{NewModule, lists:keyreplace(File, 1, NewEntry)}
			end
	end.

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
get_program(ProgramFile) ->
	gen_server:call(?MODULE, {get_program, ProgramFile}).
get_model(ModelFile) ->
	get_server:call(?MODULE, {get_model, ModelFile}).
get_view(ViewFile) ->
	get_server:call(?MODULE, {get_view, ViewFile}).
get_controller(ControllerFile) ->
	get_controller:call(?MODULE, {get_controller, ControllerFile}).
