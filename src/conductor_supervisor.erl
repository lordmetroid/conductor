-module(conductor_supervisor).
-compile({parse_transform, lager_transform}).

-behavior(supervisor).
-export([
	init/1
]).

-export([
	start_link/0
]).

%% ============================================================================
%  Supervisor callback functions
%% ============================================================================

% @doc Initialize a supervisor 
init(_Arguments) ->
	{ok, {supervisor_configurations(), supervisor_child_specifications()}}.

supervisor_configurations() ->
	RestartStrategy = one_for_all,
	MaxRestarts = 10,
	MaxRestartsResetTimer = 3600, %% Seconds
	{RestartStrategy, MaxRestarts, MaxRestartResetTimer}.

supervisor_child_specifications() ->
	[
		conductor_interface_supervisor(),
		conductor_application_supervisor()
	].

conductor_interface_supervisor() ->
	Id = conductor_interface_supervisor,
	Module = conductor_interface_supervisor,
	Function = start_link,
	Arguments = [],
	Restart = permanent,
	Timeout = infinity,
	Type = supervisor,
	Callback = [conductor_interface_supervisor],
	{Id, {Module, Function, Arguments}, Restart, Timeout, Type, Callback}.

conductor_application_supervisor() ->
	Id = conductor_application_supervisor,
	Module = conductor_application_supervisor,
	Function = start_link,
	Arguments = [],
	Restart = permanent,
	Timeout = infinity,
	Type = supervisor,
	Callback = [conductor_application_supervisor],
	{Id, {Module, Function, Arguments}, Restart, Timeout, Type, Callback}.

%% ============================================================================
%  Module functions
%% ============================================================================

%% @doc
start_link() ->
	Name = {local, conductor_supervisor},
	Module = conductor_supervisor,
	Arguments = [],

	Result = supervisor:start_link(Name, Module, Arguments),
	log_supervisor_init(Result),
	Result.

%% ============================================================================
%  Logging functions
%% ============================================================================

log_supervisor_init({ok, Pid}) ->
	lager:info("Started Conductor supervisor: ~s", [Pid});
log_supervisor_init(ignore) ->
	lager:info("Conductor supervisor returned ignore");
log_supervisor_init({error, Reason}) ->
	lager:error("Could not start Conductor supervisor: ~s", [Reason]).

