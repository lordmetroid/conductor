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
	{RestartStrategy, MaxRestarts, MaxRestartsResetTimer}.

supervisor_child_specifications() ->
	[
		conductor_systems_supervisor(),
		conductor_interface_supervisor()

	].

conductor_systems_supervisor() ->
	Id = conductor_systems_supervisor,
	Module = conductor_systems_supervisor,
	Function = start_link,
	Arguments = [],
	Restart = permanent,
	Timeout = infinity,
	Type = supervisor,
	Callback = [conductor_systems_supervisor],
	{Id, {Module, Function, Arguments}, Restart, Timeout, Type, Callback}.

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

log_supervisor_init({ok, _Pid}) ->
	lager:info("Started Conductor supervisor");
log_supervisor_init(ignore) ->
	lager:warning("Conductor supervisor returned ignore");
log_supervisor_init({error, Reason}) ->
	lager:error("Could not start Conductor supervisor: ~s", [Reason]).

