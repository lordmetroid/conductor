-module(conductor_cache).
-compule({parse_transform, lager_transform}).

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
	get_module/2
]).

%% ----------------------------------------------------------------------------
% @spec
% @doc Compile and cache all web application files available at start
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	%% Initial cache and garbage heap
	{ok, {[],[]}}.

handle_call({get_module, Type, ModulePath}, _From, Modules) ->
	 {Module, UpdatedModules} = cache_get_module(Type, ModulePath, Modules),
	 {reply, Module, UpdatedModules};
	
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


%% ============================================================================
%% Module functions
%% ============================================================================

%% @doc Start the cache manager
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get a cached module
get_module(Type, ModulePath) ->
	gen_server:call(?MODULE, {get_module, Type, ModulePath}).

cache_get_module(Type, ModulePath, Modules) ->
	{Cache, Garbage} = Modules,

	case lists:keyfind(ModulePath, 1, Cache) of
		false ->
			add_module_to_cache(Type, ModulePath, Modules);
		{ModulePath, {Module, Date}} ->
			check_file_updates(Type, ModulePath, Modules, {Module, Date})
	end.

add_module_to_cache(Type, ModulePath, {Cache, Garbage}) ->
	case install_module(Type, ModulePath) of
		error ->
			log_add_module_error(ModulePath),
			{false, {Cache, Garbage}};
		{Module, Date} ->
			UpdatedCache = [{ModulePath, {Module,Date}} | Cache],
			{Module, {UpdatedCache, Garbage}}
	end.


check_file_updates(Type, ModulePath, {Cache, Garbage}, {Module, Date}) ->
	case filelib:last_modified(ModulePath) of
		0 ->
			%% Module file has been deleted
			remove_deleted_module(ModulePath, {Cache, Garbage}, Module);
		Date ->
			%% Module in cache is up to date
			{Module, {Cache, Garbage}};
		_NewDate ->
			%% Module file has been changed since last cached
			update_cache(Type, ModulePath, {Cache, Garbage}, Module)
	end.

remove_deleted_module(ModulePath, {Cache, Garbage}, Module) ->
	case code:delete(Module) of
		false ->
			log_delete_module_error(ModulePath),
			{false, {Cache, Garbage}};
		true ->
			UpdatedCache = lists:keydelete(ModulePath,1, Cache),
			UpdatedGarbage = uninstall_module([Module | Garbage], []),
			
			{false, {UpdatedCache, UpdatedGarbage}}
	end.

update_cache(Type, ModulePath, {Cache, Garbage}, Module) ->
	case install_module(Type, ModulePath) of
		error ->
			log_update_module_error(ModulePath),
			{false, {Cache, Garbage}};
		{NewModule, Date} ->
			NewEntry = {ModulePath, {NewModule, Date}},
			UpdatedCache = lists:keyreplace(ModulePath, 1, Cache, NewEntry),

			code:delete(Module),
			UpdatedGarbage = uninstall_module([Module | Garbage], []),

			{NewModule, {UpdatedCache, UpdatedGarbage}}
	end.

%% ============================================================================
%% Helper functions
%% ============================================================================

%% @doc Compile and load a module into the virtual machine
%% @spec
install_module(Type, ModulePath) ->
	case filelib:last_modified(ModulePath) of
		0 ->
			log_file_not_found_error(ModulePath),
			error;
		Date ->
			compile_module(Type, ModulePath, Date)
	end.

compile_module(Type, ModulePath, Date) ->
	case conductor_cache_compiler:make(Type, ModulePath) of
		error ->
			log_compile_error(ModulePath),
			error;
		{Module, ModuleBinary} ->
			load_module_binary(ModulePath, Date, Module, ModuleBinary)
	end.

load_module_binary(ModulePath, Date, Module, ModuleBinary) ->
	case code:load_binary(Module, Module, ModuleBinary) of
		{error, Reason} ->
			log_load_binary_error(ModulePath, Reason),
			error;
		{module, Module} ->
			{Module, Date}
	end.

%% @doc Unload modules from the virtual machine
%% @spec 
uninstall_module([], UpdatedGarbage) ->
	UpdatedGarbage;
uninstall_module([Module | Rest], UpdatedGarbage) ->
	case code:soft_purge(Module) of
		false ->
			code:delete(Module),
			uninstall_module(Rest, [Module | UpdatedGarbage]);
		true ->
			uninstall_module(Rest, UpdatedGarbage)
	end.

%% ============================================================================
%% Logging functions
%% ============================================================================

log_add_module_error(ModulePath) ->
	lager:warning("Could not add ~s to the cache", [ModulePath]).

log_update_module_error(ModulePath) ->
	lager:warning("Could not update cache for ~s", [ModulePath]).

log_delete_module_error(ModulePath) ->
	lager:warning("Could not delete ~s from cache", [ModulePath]).

log_file_not_found_error(ModulePath) ->
	lager:warning("Could not find module file: ~s", [ModulePath]).

log_compile_error(ModulePath) ->
	lager:warning("Could not compile: ~s", [ModulePath]).

log_load_binary_error(ModulePath, Reason) ->
	lager:warning("Could not load load binary, ~s: ~p", [ModulePath, Reason]).
