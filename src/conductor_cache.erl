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

handle_call({get_module, ModulePath}, _From, Cache) ->
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
					case update_cache(ModulePath, {Module,Date}, Cache) of
						{error, Errors} ->
							{reply, {error, Errors, Warnings}, Cache};	
						{ok, UpdatedCache} ->
							{reply, {ok, Module, Warnings}, UpdatedCache}
					end;
				{ok, {Module,Date}} ->
					%% New compiled module
					case update_cache(ModulePath, {Module,Date}, Cache) of
						{error, Errors} ->
							{reply, {error, Errors, Warnings}, Cache};	
						{ok, UpdatedCache} ->
							{reply, {ok, Module}, UpdatedCache}
					end
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
					{ok, lists:keyreplace(ModulePath,1, Cache, {Module,Date})}
			end
	end.

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
get_program(ProgramFile) ->
	ProgramRoot = conductor_settings:get(program_root),
	ProgramPath = filename:join([ProgramRoot, ProgramFile]),

	gen_server:call(?MODULE, {get_module, ProgramPath}).

get_model(ModelFile) ->
	ModelRoot = conductor_settings:get(model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),

	gen_server:call(?MODULE, {get_module, ModelPath}).

get_view(ViewFile) ->
	ViewRoot = conductor_settings:get(view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),

	gen_server:call(?MODULE, {get_module, ViewPath}).

get_controller(ControllerFile) ->
	ControllerRoot = conductor_settings:get(controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),

	gen_server:call(?MODULE, {get_module, ControllerPath}).

