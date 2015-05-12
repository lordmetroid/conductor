-module(conductor_application_supervisor).

-behavior(supervisor).
-export([
	init/1
]).

-export([
	start_link/0
]).

%% ============================================================================
%  Supervisor callbacks
%% ============================================================================

%% @doc Initialize a supervisor 
init(_Arguments) ->
	{ok, supervisor_configurations(), supervisor_child_specification()}.

supervisor_configurations() ->
	RestartStrategy = one_for_one, %% Only restart the crashed child
	MaxRestarts = 10,
	MaxRestartsResetTimer = 3600, %% Seconds
	{RestartStrategy, MaxRestarts, MaxRestartsResetTimer}.



supervisor_child_specifications() ->
	[
		conductor_settings_worker(),
		conductor_cache_worker(),
		conductor_response_worker()
	].

conductor_settings_worker() ->
	Id = conductor_settings,
	Module = conductor_settings,
	Function = start_link,
	Arguments = [],
	Restart = permanent,
	Timeout = 7200,
	Type = worker,
	Callback = [conductor_settings],
	{Id, {Module, Function, Arguments}, Restart, Timeout, Type, Callback}.

conductor_cache_worker() ->
	Id = conductor_cache,
	Module = conductor_cache,
	Function = start_link,
	Arguments = [],
	Restart = permanent,
	Timeout = 7200,
	Type = worker,
	Callback = [conductor_cache],
	{Id, {Module, Function, Arguments}, Restart, Timeout, Type, Callback}.

conductor_response_worker() ->
	Id = conductor_response,
	Module = conductor_response,
	Function = start_link,
	Arguments = [],
	Restart = permanent,
	Timeout = 7200,
	Type = worker,
	Callback = [conductor_response],
	{Id, {Module, Function, Arguments}, Restart, Timeout, Type, Callback}.

%% ============================================================================
%  Module functions
%% ============================================================================


%% @doc
start_link() ->
	Name = {local, conductor_application_supervisor},
	Module = conductor_application_supervisor,
	Arguments = [],

	Result = supervisor:start_link(Name, Module, Arguments),
	log_supervisor_init(Result),
	Result.

%% ============================================================================
%  Logging functions
%% ============================================================================

log_supervisor_init({ok, Pid}) ->
	lager:info("Started Conductor application supervisor: ~s", [Pid]);
log_supervisor_init(ignore) ->
	lager:info("Conductor application supervisor returned ignore");
log_supervisor_init({error, Reason}) ->
	lager:error("Could not start Conductor application supervisor: ~s", [Reason]).

