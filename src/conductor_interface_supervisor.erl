-module(conductor_interface_supervisor).
-compile({parse_transform, lager_transform}).

-behaviour(supervisor).
-export([
	init/1
]).

-export([
	start_link/0
]).

%% ============================================================================
%  Supervisor callback functions
%% ============================================================================

%% @doc
init(_Arguments) ->
	{ok, {supervisor_configurations(), supervisor_child_specifications()}}.

supervisor_configurations() ->
	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxRestartsResetTimer = 3600, %% Seconds
	{RestartStrategy, MaxRestarts, MaxRestartsResetTimer}.

supervisor_child_specifications() ->
	[
		conductor_interface()
	].

conductor_interface() ->
	Id = conductor_interface,
	Module = webmachine_mochiweb,
	Function = start,
	Arguments = [conductor_interface_configurations()],
	Restart = permanent,
	Timeout = 7200, %% Seconds
	Type = worker,
	Callback = dynamic,
	{Id, {Module, Function, Arguments}, Restart, Timeout, Type, Callback}.

conductor_interface_configurations() ->
	[
		%% Server configurations
		{ip, "127.0.0.1"},
		{port, 8000}, 

		%% Interfaces
		{dispatch, [
			{['*'], conductor_router_interface, []}
		]}
	].

%% ============================================================================
%  Module functions
%% ============================================================================
start_link() ->
	Name = {local, conductor_interface_supervisor},
	Module = conductor_interface_supervisor,
	Arguments = [],

	supervisor:start_link(Name, Module, Arguments).

