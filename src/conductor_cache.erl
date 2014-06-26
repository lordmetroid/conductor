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
	%% TODO: Compile and cache modules existing at start
	{ok, []}.

handle_call({get_module, ModulePath}, _From, Cache) ->
	case lists:keyfind(ModulePath,1, Cache) of
		false ->
			%% Module does not exist in cache
			case add_module(Cache, ModulePath) of
				error ->
				{ok, Module, UpdatedCache} ->
			end;
		{ModulePath, {Module,Date}} ->
			case filelib:last_modified(ModulePath) of
				0 ->
					%% Module file has been deleted
					case remove_module(Cache, ModulePath) of
						error ->
							%% Module could not be removed from cache
							{reply, error, Cache}
						{ok, UpdatedCache} ->
							%% Module removed from cache
							{Reply error, UpdatedCache};
					end;
				Date ->
					%% Module in cache is up to date
					{reply, {ok, Module}, Cache};
				NewDate ->
					%% Module file has been changed since last cached
					case update_module(Cache, ModulePath) of
						error ->
							%% Cache could not be updated
							{reply, error, Cache};
						{ok, Module, UpdatedCache} ->
							%% Updated cache with new module
							{reply, {ok, Module}, UpdatedCache}
					end
			end
	end.
	
add_module(Cache, ModulePath) ->
	case compile_module(ModulePath) of
		error ->
			%% Module could not be compiled
			error;
		{ok, {Module,Date}} ->
			%% Add module to cache
			{ok, Module, [{ModulePath, {Module,Date}} | Cache]}
	end.

update_module(Cache, ModulePath) ->
	case compile_module(ModulePath) of
		error ->
			%% Module could not be compiled
			error;
		{ok, {Module,Date}} ->
			%% Update cache with new module
			NewModule = {ModulePath, {Module,Date}},
			{ok, Module, lists:keyreplace(ModulePath,1, Cache, NewModule)}
	end.

compile_module(ModulePath) ->
	%% Get data of module file
	case filelib:last_modified(ModulePath) of
		0 ->
			%% Module file does not exist
			conductor_log:add(ModulePath, "File not found"),
			error;
		Date ->
			case conductor_compiler:make(ModulePath) of
				error ->
					%% Could not compile module
					conductor_log:add(ModulePath, "Could not be compiled"),
					error;
				{ok, Module, ModuleBinary}
					%% TODO: Load compiled module
					{ok, {Module, Date}}
			end
	end.

remove_module(Cache, ModulePath) ->

	
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
get_program(ProgramPath) ->
	gen_server:call(?MODULE, {get_module, ProgramPath}).

get_model(ModelPath) ->
	gen_server:call(?MODULE, {get_module, ModelPath}).

get_view(ViewPath) ->
	gen_server:call(?MODULE, {get_module, ViewPath}).

get_controller(ControllerPath) ->
	gen_server:call(?MODULE, {get_module, ControllerPath}).

