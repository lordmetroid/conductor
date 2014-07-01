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
	%% TODO: Compile and cache modules existing at start
	{ok, {[],[]}}.

handle_call({get_module, ModulePath}, _From, Modules) ->
	case lists:keyfind(ModulePath,1, Modules) of
		false ->
			%% Module does not exist in cache
			case add(Modules, ModulePath) of
				error ->
					%% Could not add module
					{reply, error, Modules};
				{ok, UpdatedModules, NewModule} ->
					%% New module added to cache
					{reply, {ok, NewModule}, UpdatedModules}
			end;
		{ModulePath, {Module,Date}} ->
			case filelib:last_modified(ModulePath) of
				0 ->
					%% Module file has been deleted
					case remove(Modules, Module, ModulePath) of
						error ->
							%% Cache has not been changed
							{reply, error, Modules};
						{ok, UpdatedModules} ->
							%% Module removed from cache
							{reply, error, UpdatedModules}
					end;
				Date ->
					%% Module in cache is up to date
					{reply, {ok, Module}, Modules};
				_NewDate ->
					%% Module file has been changed since last cached
					case update(Modules, Module, ModulePath) of
						error ->
							%% Cache could not be updated
							{reply, error, Modules};
						{ok, UpdatedModules, NewModule} ->
							%% Updated cache with new module
							{reply, {ok, NewModule}, UpdatedModules}
					end
			end
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
% get_module helper function
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec add({Cache, DeathRow}, ModulePath) -> error | 
% @doc Add module to cache
%% ----------------------------------------------------------------------------
add({Cache, DeathRow}, ModulePath) ->
	case install_module(ModulePath) of
		error ->
			%% Module could not be compiled
			error;
		{ok, {Module,Date}} ->
			%% Add module to cache
			UpdatedCache = [{ModulePath, {Module,Date}} | Cache],
			{ok, {UpdatedCache, DeathRow}, Module}
	end.

%% ----------------------------------------------------------------------------
% @spec purge_module(DeathRow) -> UpdatedDeathRow
% @doc Purge all lingering modules from memory
%% ----------------------------------------------------------------------------
update({Cache, DeathRow}, Module, ModulePath) ->
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
			UpdatedDeathRow = uninstall_module([Module | DeathRow]),
			
			{ok, {UpdatedCache, UpdatedDeathRow}, Module}
	end.

%% ----------------------------------------------------------------------------
% @spec purge_module(DeathRow) -> UpdatedDeathRow
% @doc Purge all lingering modules from memory
%% ----------------------------------------------------------------------------
remove({Cache, DeathRow}, Module, ModulePath) ->
	%% Block processes from executing old code
	case code:delete(Module) of
		false ->
			%% Could not delete module, even older code not yet purged
			error;
		true ->
			%% Remove module from cache
			UpdatedCache = lists:keydelete(ModulePath,1, Cache),
			
			%% Purge modules lingering in memory
			UpdatedDeathRow = uninstall_module([Module | DeathRow]),
			
			{ok, {UpdatedCache, UpdatedDeathRow}}
	end.

%% ----------------------------------------------------------------------------
% @spec purge_module(DeathRow) -> UpdatedDeathRow
% @doc Purge all lingering modules from memory
%% ----------------------------------------------------------------------------
uninstall_module(DeathRow) ->
	uninstall_module(DeathRow, []).

uninstall_module([Module | Rest], DeathRow) ->
	case code:soft_purge(Module) of
		false ->
			%% Module still occupied by process
			code:delete(Module),
			uninstall_module(Rest, [Module | DeathRow]);
		true ->
			%% Module successfully purged from memory
			uninstall_module(Rest, DeathRow)
	end;
uninstall_module([], DeathRow) ->
	%% Return updated deathrow
	DeathRow.

%% ----------------------------------------------------------------------------
% @spec install_module(ModulePath) -> error | {ok, {Module, Date}}
% @doc Compile and load module from file into memory
%% ----------------------------------------------------------------------------
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
					case code:load_module(Module, ModuleBinary) of
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
