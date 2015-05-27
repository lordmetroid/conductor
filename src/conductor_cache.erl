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
	get_module/1
]).

%% ----------------------------------------------------------------------------
% @spec
% @doc Compile and cache all web application files available at start
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	%% Initial cache and garbage heap
	{ok, {[],[]}}.

handle_call({get_module, ModulePath}, _From, Modules) ->
	 {UpdatedModules, Module} = cache_get_module(ModulePath, Modules),
	 {reply, Module, UpdatedModules}
	
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
% get_module helper function
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec add({Cache, DeathRow}, ModulePath) -> error | 
% @doc Add module to cache
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec purge_module(DeathRow) -> UpdatedDeathRow
% @doc Purge all lingering modules from memory
%% ----------------------------------------------------------------------------
update({Cache, Garbage}, ModulePath, Module) ->
	case install_module(ModulePath) of
		error ->
			%% Module could not be compiled
			error;
		{ok, {NewModule,Date}} ->
			%% Update cache with new module
			NewEntry = {ModulePath, {NewModule,Date}},
			UpdatedCache = lists:keyreplace(ModulePath,1, Cache, NewEntry),
			
			%% Purge modules lingering in memory
			code:delete(Module),
			UpdatedGarbage = uninstall_module([Module | Garbage]),
			
			{ok, {UpdatedCache, UpdatedGarbage}, NewModule}
	end.

%% ----------------------------------------------------------------------------
% @spec purge_module(DeathRow) -> UpdatedDeathRow
% @doc Purge all lingering modules from memory
%% ----------------------------------------------------------------------------
remove({Cache, Garbage}, ModulePath, Module) ->
	%% Block processes from executing old code
	case code:delete(Module) of
		false ->
			%% Could not delete module, even older code not yet purged
			error;
		true ->
			%% Remove module from cache
			UpdatedCache = lists:keydelete(ModulePath,1, Cache),
			
			%% Purge modules lingering in memory
			UpdatedGarbage = uninstall_module([Module | Garbage]),
			
			{ok, {UpdatedCache, UpdatedGarbage}}
	end.

%% ----------------------------------------------------------------------------
% @spec purge_module(DeathRow) -> UpdatedDeathRow
% @doc Purge all lingering modules from memory
%% ----------------------------------------------------------------------------
uninstall_module(NewGarbage) ->
	uninstall_module(NewGarbage, []).

uninstall_module([Module | Rest], UpdatedGarbage) ->
	case code:soft_purge(Module) of
		false ->
			%% Module still occupied by process
			code:delete(Module),
			uninstall_module(Rest, [Module | UpdatedGarbage]);
		true ->
			%% Module successfully purged from memory
			uninstall_module(Rest, UpdatedGarbage)
	end;
uninstall_module([], UpdatedGarbage) ->
	%% Return updated deathrow
	UpdatedGarbage.

%% ----------------------------------------------------------------------------
% @spec install_module(ModulePath) -> error | {ok, {Module, Date}}
% @doc Compile and load module from file into memory
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec start() ->
% @doc Start the web application file cache manager
%% ----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
% @spec get_module(ModulePath) -> error | {ok, Module}
% @doc Get the cached web application module
%% ----------------------------------------------------------------------------
get_module(ModulePath) ->
	gen_server:call(?MODULE, {get_module, ModulePath}).

cache_get_module(ModulePath, {Cache, Garbage}) ->
	case lists:keyfind(ModulePath,1, Cache) of
		false ->
			add_module_to_cache(ModulePath, {Cache, Garbage});
		{ModulePath, {Module, Date}} ->
			check_file_updates(ModulePath, Modules, Module, Date) 
	end.

add_module_to_cache(ModulePath, {Cache, Garbage}) ->
	case install_module(ModulePath) of
		error ->
			
		{Module, Date} ->
			UpdatedCache = [{ModulePath, {Module, Date}} | Cache],
			{Module, {UpdatedCache, Garbage}}
	end.


check_file_updates(ModulePath, {Cache, Garbage}, Module, Date) ->
	case filelib:last_modified(ModulePath) of
		0 ->
			%% Module file has been deleted
			remove_deleted_module(ModulePath, {Cache, Garbage}, Module),
		Date ->
			%% Module in cache is up to date
			{Module, {Cache, Garbage}};
		_NewDate ->
			%% Module file has been changed since last cached
			update_cache(ModulePath, {Cache, Garbage}, Module)
	end.

update_cache(ModulePath, {Cache, Garbage}, Module) ->
	case install_module(ModulePath) of
		error ->
		{NewModule, Date} ->
			NewEntry = {ModulePath, {NewModule, Data}},
			UpdatedCache = lists:keyreplace(ModulePath, 1, Cache, NewEntry),

			code:delete(Module),
			UpdatedGarbage = uninstall_module(Module, Garbage),

			{{UpdatedCache, UpdatedGarbage}, NewModule}
	end.

remove_deleted_module(ModuklePath, {Cache, Garbage}, Module) ->
			case remove(Modules, ModulePath, Module) of
				error ->
					%% Cache has not been changed
					{reply, error, Modules};
				{ok, UpdatedModules} ->
					%% Module removed from cache
					{reply, error, UpdatedModules}
			end;

add({Cache, Garbage}, ModulePath) ->
	case install_module(ModulePath) of
		error ->
			%% Module could not be compiled
			error;
		{Module,Date} ->
			%% Add module to cache
			UpdatedCache = [{ModulePath, {Module,Date}} | Cache],
			{ok, {UpdatedCache, Garbage}, Module}
	end.

install_module(ModulePath) ->


	case filelib:last_modified(ModulePath) of
		0 ->
			%% Module file does not exist
			conductor_log:add(ModulePath, "File not found"),
			error;
		Date ->
			%% Compile and load module
			case conductor_compiler:make(ModulePath) of
				error ->
					%% Could not compile module
					conductor_log:add(ModulePath, "Could not be compiled"),
					error;
				{ok, Module, ModuleBinary} ->
					%% Load compiled module
					case code:load_binary(Module, Module, ModuleBinary) of
						{error, Errors} ->
							%% Module could not be loaded
							conductor_log:add(ModulePath,
								"Could not load code"),
							error;
						{module, Module} ->
							%% Module loaded
							{ok, {Module, Date}}
					end
			end
	end.


%% ============================================================================
%% Helper functions
%% ============================================================================

%% @doc Compile and load a module into the virtual machine
%% @spec
install_module(ModulePath) ->
	case filelib:last_modified(ModulePath) of
		0 ->
			log_file_not_found_error(ModulePath),
			error;
		Date ->
			compile_module(ModulePath, Date)
	end.

compile_module(ModulePath, Date) ->
	case conductor_compiler:make(ModulePath) of
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

%% ============================================================================
%% Logging functions
%% ============================================================================

log_file_not_found_error(ModulePath) ->
	lager:warning("Could not find module file: ~s", [ModulePath]).

log_compile_error(ModulePath) ->
	lager:warning("Could not compile: ~s", [ModulePath]).

log_load_binary_error(ModulePath, Reason) ->
	lager:warning("Could not load load binary, ~s: ~p", [ModulePath, Reason]).
