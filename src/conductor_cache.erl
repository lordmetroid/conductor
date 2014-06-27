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

handle_call({get_module, ModulePath}, _From, Cache) ->
	case lists:keyfind(ModulePath,1, Cache) of
		false ->
			%% Module does not exist in cache
			case add(Cache, ModulePath) of
				error ->
					%% Could not add module
					{reply, error, Cache};
				{ok, UpdatedCache, NewModule} ->
					%% New module added to cache
					{reply, {ok, Module}, UpdatedCache}
			end;
		{ModulePath, {Module,Date}} ->
			case filelib:last_modified(ModulePath) of
				0 ->
					%% Module file has been deleted
					case remove(Cache, Module, ModulePath) of
						error ->
							%% Cache has not been changed
							{reply, error, Cache}
						{ok, UpdatedCache} ->
							%% Module removed from cache
							{Reply, error, UpdatedCache};
					end;
				Date ->
					%% Module in cache is up to date
					{reply, {ok, Module}, Cache};
				NewDate ->
					%% Module file has been changed since last cached
					case update(Cache, Module ModulePath) of
						error ->
							%% Cache could not be updated
							{reply, error, Cache};
						{ok, UpdatedCache, NewModule} ->
							%% Updated cache with new module
							{reply, {ok, NewModule}, UpdatedCache}
					end
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

add({Cache, DeathRow}, ModulePath) ->
	case install_module(ModulePath) of
		error ->
			%% Module could not be compiled
			error;
		{ok, {Module,Date}} ->
			%% Add module to cache
			{ok, Module, {[{ModulePath, {Module,Date}} | Cache], DeathRow}}
	end.

%% ----------------------------------------------------------------------------
% get_module helper function
%% ----------------------------------------------------------------------------
update({Cache, DeathRow}, Module, ModulePath) ->
	case install_module(ModulePath) of
		error ->
			%% Module could not be compiled
			error;
		{ok, {Module,Date}} ->
			%% Update cache with new module
			NewModule = {ModulePath, {Module,Date}},
			UpdatedCache = lists:keyreplace(ModulePath,1, Cache, NewModule),
			
			{ok, {{UpdatedCache, DeathRow}, Module}
	end.
	
remove({Cache, DeathRow}, Module, ModulePath) ->
	case 

	%% Block processes from executing old code
	case code:delete(Module) of
		false ->
			%% 
		true ->
			%% Purge modules lingering in memory
			UpdatedDeathRow = purge_module([Module | DeathRow]),
			
			%% Remove module from cache
			UpdatedCache = lists:keydelete(ModulePath,1, Cache),
			{ok, {UpdatedCache, UpdatedDeathRow}
	end.

%% ----------------------------------------------------------------------------
% @spec purge_module(DeathRow) -> UpdatedDeathRow
% @doc Purge all lingering modules from memory
%% ----------------------------------------------------------------------------
purge_module(DeathRow) ->
	purge_module(DeathRow, []).
purge_module([Module | Rest], DeathRow) ->
	case code:soft_purge(Module) of
		false ->
			%% Module still occupied by process
			purge_module(Rest, [Module | DeathRow]);
		true ->
			%% Module successfully purged from memory
			purge_module(Rest, DeathRow)
	end;
purge_module([], DeathRow) ->
	%% Return updated deathrow
	DeathRow.

%% ----------------------------------------------------------------------------
% @spec load_module(ModulePath) -> error | {ok, {Module, Date}}
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
% @spec get_...() ->
% @doc Get the cached web application file
%% ----------------------------------------------------------------------------
get_module(ProgramPath) ->
	gen_server:call(?MODULE, {get_module, ModulePath}).


