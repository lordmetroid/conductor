-module(conductor_application_supervisor).

-behavior(supervisor).
-export([
	init/1
]).

-export([
	start_link/0
]).

%% ----------------------------------------------------------------------------
% Supervisor callbacks
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec init(_Arguments) -> {ok, SupervisorSpecifications}
% @doc Initialize a supervisor 
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	SupervisorSpecifications = create_supervisor_specifications(),
	{ok, SupervisorSpecifications}.

create_supervisor_specifications() ->
	SupervisorSettings = supervisor_settings(),
	ChildSpecifications = supervisor_child_specifications(),
	{SupervisorSettings, ChildSpecifications}.

supervisor_settings() ->
	RestartStrategy = supervisor_restart_strategy(),
	MaxRestarts = supervisor_max_restarts(),
	MaxRestartsResetTimer = supervisor_max_restarts_reset_timer(),
	{RestartStrategy, MaxRestarts, MaxRestartsResetTimer}.
	
supervisor_restart_strategy() ->
	one_for_one. %% Only restart the crashed child
supervisor_max_restarts() ->
	10.
supervisor_max_restarts_reset_timer() ->
	3600. %% Seconds


supervisor_child_specifications() ->
	ConductorSupervisorChild = conductor_supervisor_child(),
	WebInterfaceSupervisorChild = conductor_web_interface_supervisor_child(),
	[ConductorSupervisorChild, WebInterfaceSupervisorChild].

%% Conductor systems supervisor
conductor_supervisor_child() ->
	Id = conductor_supervisor_id(),
	Start = conductor_supervisor_start(),
	Restart = conductor_supervisor_restart(),
	ShutdownTimeout = conductor_supervisor_shutdown_timeout(),
	Type = conductor_supervisor_type(),
	CallbackModule = conductor_supervisor_callback_module(),
	{Id, Start, Restart, ShutdownTimeout, Type, CallbackModule}.

conductor_supervisor_id() ->
	conductor_supervisor.
conductor_supervisor_start() ->
	StartModule = conductor_supervisor_start_module(),
	StartFunction = conductor_supervisor_start_function(),
	StartArguments = conductor_supervisor_start_arguments(),
	{StartModule, StartFunction, StartArguments}.

conductor_supervisor_start_module() ->
	conductor_supervisor.
conductor_supervisor_start_function() ->
	start_link.
conductor_supervisor_start_arguments() ->
	[].

conductor_supervisor_restart() ->
	permanent. %% Always restart child
conductor_supervisor_shutdown_timeout() ->
	infinity.
conductor_supervisor_type() ->
	supervisor.
conductor_supervisor_callback_module() ->
	[conductor_supervisor].

%% Web interface supervisor
conductor_web_interface_supervisor_child() ->
	Id = conductor_web_interface_supervisor_id(),
	Start = conductor_web_interface_supervisor_start(),
	Restart = conductor_web_interface_supervisor_restart(),
	ShutdownTimeout = conductor_web_interface_supervisor_shutdown_timeout(),
	Type = conductor_web_interface_supervisor_type(),
	CallbackModule = conductor_web_interface_supervisor_callback_module(),
	{Id, Start, Restart, ShutdownTimeout, Type, CallbackModule}.

conductor_web_interface_supervisor_id() ->
	conductor_web_interface_supervisor.
conductor_web_interface_supervisor_start() ->
	StartModule = conductor_web_interface_supervisor_start_module(),
	StartFunction = conductor_web_interface_supervisor_start_function(),
	StartArguments = conductor_web_interface_supervisor_start_arguments(),
	{StartModule, StartFunction, StartArguments}.

conductor_web_interface_supervisor_start_module() ->
	conductor_web_interface_supervisor.
conductor_web_interface_supervisor_start_function() ->
	start_link.
conductor_web_interface_supervisor_start_arguments() ->
	[].

conductor_web_interface_supervisor_restart() ->
	permanent. %% Always restart child
conductor_web_interface_supervisor_shutdown_timeout() ->
	infinity.
conductor_web_interface_supervisor_type() ->
	supervisor.
conductor_web_interface_supervisor_callback_module() ->
	[conductor_web_interface_supervisor].

%% ----------------------------------------------------------------------------
% Module functions
%% ----------------------------------------------------------------------------

% @doc Start supervisor if it is not already running
% @spec
start_link() ->
	Result = start_link_application_supervisor(),
	handle_application_supervisor_start(Result).

	start_link_application_supervisor() ->
	Name = application_supervisor_name_registration(),
	Module = application_supervisor_module(),
	InitArguments = application_supervisor_init_arguments(),
	supervisor:start_link(Name, Module, InitArguments).

application_supervisor_name_registration() ->
	{local, conductor_application_supervisor}.
application_supervisor_module() ->
	conductor_application_supervisor.
application_supervisor_init_arguments() ->
	[].

handle_application_supervisor_start({ok, Pid}) ->
	log_info("Conductor application supervisor started", Pid),
	{ok, Pid};
handle_application_supervisor_start(ignore) ->
	log_info("Conductor application supervisor returned ignore"),
	ignore;
handle_application_supervisor_start({error, Error}) ->
	log_error("ERROR: Can not start Conductor application supervisor", Error),
	{error, Error}.

%% ----------------------------------------------------------------------------
% Logging functions
%% ----------------------------------------------------------------------------

log_info(Message) ->
	lager:info(Message).
log_info(Message, Arguments) ->
	lager:info(Message, Arguments).

log_error(Message, Arguments) ->
	lager:error(Message, Arguments). 


