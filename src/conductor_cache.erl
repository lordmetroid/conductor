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
	%% TODO: Add error reporting
	
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

	case get_module(ProgramPath, Cache) of
		{error, Errors} ->
			%% Report errors
			{reply, {error, Errors}, Cache};
		{error, Errors, Warnings} ->
			%% Report errors and warnings
			{reply, {error, Errors, Warnings}, Cache};
		{ok, {Program,Date}, Warnings} ->
			%% Update cache with new module despite warnings
			case update_cache(ProgramPath, {Program,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, Program, Warnings}, UpdatedCache}
			end;
		{ok, {Program,Date}} ->
			%% Update cache with new module
			case update_cache(ProgramPath, {Program,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, Program}, UpdatedCache}
			end;
	end;

handle_call({get_model, ModelFile}, _From, Cache) ->
	ModelRoot = conductor_settings:get(model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),

	case get_module(ModelPath, Cache) of
		{error, Errors} ->
			%% Report errors
			{reply, {error, Errors}, Cache};
		{error, Errors, Warnings} ->
			%% Report errors and warnings
			{reply, {error, Errors, Warnings}, Cache};
		{ok, {Model,Date}, Warnings} ->
			%% Update cache with new module despite warnings
			case update_cache(ModelPath, {Model,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, Model, Warnings}, UpdatedCache}
			end;
		{ok, {Model,Date}} ->
			%% Update cache with new module
			case update_cache(ModelPath, {Model,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, Model}, UpdatedCache}
			end;
	end;

handle_call({get_view, ViewFile}, _From, Cache) ->
	ViewRoot = conductor_settings:get(view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),

	case get_module(ViewPath, Cache) of
		{error, Errors} ->
			%% Report errors
			{reply, {error, Errors}, Cache};
		{error, Errors, Warnings} ->
			%% Report errors and warnings
			{reply, {error, Errors, Warnings}, Cache};
		{ok, {View,Date}, Warnings} ->
			%% Update cache with new module despite warnings
			case update_cache(ViewPath, {View,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, View, Warnings}, UpdatedCache}
			end;
		{ok, {View,Date}} ->
			%% Update cache with new module
			case update_cache(ViewPath, {View,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, View}, UpdatedCache}
			end;
	end;

handle_call({get_controller, ControllerFile}, _From, Cache) ->
	ControllerRoot = conductor_settings:get(controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),

	case get_module(ControllerPath, Cache) of
		{error, Errors} ->
			%% Report errors
			{reply, {error, Errors}, Cache};
		{error, Errors, Warnings} ->
			%% Report errors and warnings
			{reply, {error, Errors, Warnings}, Cache};
		{ok, {Controller,Date}, Warnings} ->
			%% Update cache with new module despite warnings
			case update_cache(ControllerPath, {Controller,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, Controller, Warnings}, UpdatedCache}
			end;
		{ok, {Controller,Date}} ->
			%% Update cache with new module
			case update_cache(ControllerPath, {Controller,Date}, Cache),
				{error, Errors} ->
					{reply, {error, Errors, Warnings}, Cache};	
				{ok, UpdatedCache} ->
					{reply, {ok, Controller}, UpdatedCache}
			end;
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
% @spec get_module(ModulePath, Cache) -> 
% @doc 
%% ----------------------------------------------------------------------------
get_module(ModulePath, Cache) ->
	case lists:keyfind(ModulePath,1, Cache) of
		false ->
			%% Module does not exist in cache
			case conductor_compiler:make_module(ModulePath) of
				{error, Errors} ->
					%% Report compilation errors
					{error, Errors};
				{error, Errors, Warnings} ->
					%% Report compilation errors and warnings
					{error, Errors, Warnings};
				{ok, {Module,Date}, Warnings} ->
					%% Report compilation warnings of new module
					{ok, {Module,Date}, Warnings};
				{ok, {Module,Date}} ->
					%% New compiled module
					{ok, {Module,Date}}
			end;
		{ModulePath, {Module,Date}} ->
			%% Module found in cache
			{ok, {Module,Date}}
	end.

%% ----------------------------------------------------------------------------
% @spec update_cache(ModulePath, {Module,Date}, Cache) -> UpdatedCache
% @doc 
%% ----------------------------------------------------------------------------
update_cache(ModulePath, {Module,Date}, Cache) ->
	case filelib:last_modified(ModulePath,1, Cache) of
		0 ->
			%% Module file has been deleted
			case code:delete(Module) of
				false ->
					%% Failed to delete old code
					{error, "Could not update " ++ ModulePath};
				true ->
					%% Deleted file from cache
					{ok, lists:keydelete(ModulePath,1, Cache)}
			end;
		Date ->
			%% Cache is up to date
			Cache;
		NewDate ->
			%% Module file has been updated
			case code:delete(Module) of
				false ->
					%% Failed to delete old code
					{error, "Could not update " ++ ModulePath};
				true ->
					%% Update cache with newer file
					NewModule = {Module,Date},
					{ok, lists:keyreplace(ModulePath,1, Cache, NewModule)}
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
	gen_server:call(?MODULE, {get_model, ModelFile}).

get_view(ViewFile) ->
	gen_server:call(?MODULE, {get_view, ViewFile}).

get_controller(ControllerFile) ->
	gen_server:call(?MODULE, {get_controller, ControllerFile}).

