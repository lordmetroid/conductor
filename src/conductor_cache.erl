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
% @doc Compile and cache all web application files available at start
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	%% Compile and cache modules
	{ok, {
		%% Compile programs
		conductor_compiler:make_modules(filelib:wildcard(filename:join([
			conductor_settings:get(program_root), "*.erl"
		]))),
		%% Compile models
		conductor_compiler:make_modules(filelib:wildcard(filename:join([
			conductor_settings:get(model_root), "*.erl"
		]))),
		%% Compile views
		conductor_compiler:make_modules(filelib:wildcard(filename:join([
			conductor_settings:get(view_root), "*.*"
		]))),
		%% Compile controllers
		conductor_compiler:make_modules(filelib:wildcard(filename:join([
			conductor_settings:get(controller_root), "*.erl"
		])))
	}}.

handle_call({get_program, ProgramFile}, _From, Cache) ->
	ProgramRoot = conductor_settings:get(program_root),
	ProgramPath = filename:join([ProgramRoot, ProgramFile]),

	case get_module(ProgramPath, Programs) of
		false ->
			%% Program does not exist
			{reply, false, Cache};
		{Program, NewPrograms} ->
			%% Program found and cache has been updated
			{reply, Program, Cache}
	end;

handle_call({get_model, ModelFile}, _From, Cache) ->
	ModelRoot = conductor_settings:get(model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),

	case get_module(ModelPath, Models) of
		{false, NewModels} ->
			%% Model does not exist
			{reply, false, Cache};
		{Model, NewModels} ->
			%% Model found and cache has been updated
			{reply, Model, Cache}
	end;

handle_call({get_view, ViewFile}, _From, Cache) ->
	ViewRoot = conductor_settings:get(view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),

	case get_module(ViewPath, Views) of
		{false, NewViews} ->
			%% View does not exist
			{reply, false,  Cache};
		{View, NewViews} ->
			%% View found and cache has been updated
			{reply, View, Cache}
	end;

handle_call({get_controller, ControllerFile}, _From, Cache) ->
	ControllerRoot = conductor_settings:get(controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),

	case get_module(ControllerPath, Controllers) of
		error ->
			%% Controller does not exist in cache
			{reply, error,  Cache};
		{ControllerPath, {Controller, ControllerDate}} ->
			%% Controller found
			{reply, Controller, Cache}
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
	
%% ----------------------------------------------------------------------------
% @spec get_module(ModulePath, Cache) -> Module
% @doc 
%% ----------------------------------------------------------------------------
get_module(ModulePath, Cache) ->
	case lists:keyfind(ModulePath,1, Cache) of
		false ->
			%% Module does not exist in cache
			case conductor_compiler:make_module(ModulePath) of
				error ->
					%% Module could not be compile
					error;
				{Module, ModuleDate} ->
					%% New compiled module
					{Module, ModuleDate}
			end;
		{ModulePath, {Module, ModuleDate}} ->
			%% Module found in cache
			{Module, ModuleDate}
	end.

update_cache(ModulePath, {Module, ModuleDate}, Cache) ->
	case lists:keyfind(ModulePath,1, Cache) of
		0 ->
			%% Module file has been deleted
			%% TODO: Unload old module
			
			%% Remove deleted file from cache
			NewCache = lists:keydelete(ModuleFile,1, Cache),
			{false, NewCache};
		ModuleDate ->
			%% Cache is up to date
			{Module, Cache};
		NewDate ->
			%% Module file has been updated
			
			%% TODO: Unload old module
			
			%% Update cache with newer file
			case conductor_compiler:make_module(ModulePath) of
				error ->
					%% Module could not be compiled
					{false, Cache};
				NewModule ->
					%% New module compiled
					UpdatedModule = {NewModule,NewDate}
					NewCache = lists:keyreplace(ModuleFile,1, Cache, UpdatedModule),
					{NewModule, NewCache}
			end
	end


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
	gen_server:call(?MODULE, {get_model, ModelFile}).

get_view(ViewFile) ->
	gen_server:call(?MODULE, {get_view, ViewFile}).

get_controller(ControllerFile) ->
	gen_server:call(?MODULE, {get_controller, ControllerFile}).

