-module(conductor_supervisor).

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
	one_for_all. %% Restart all children if one crashes
supervisor_max_restarts() ->
	10.
supervisor_max_restarts_reset_timer() ->
	3600. %% Seconds


supervisor_child_specifications() ->
	SettingsChild = conductor_settings_worker_child(),
	CacheChild = conductor_cache_worker_child(),
	ResponseChild = conductor_response_worker_child(),
	[SettingsChild, CacheChild, ResponseChild].

%% Conductor settings worker 
conductor_settings_worker_child() ->
	Id = conductor_settings_worker_id(),
	Start = conductor_settings_worker_start(),
	Restart = conductor_settings_worker_restart(),
	ShutdownTimeout = conductor_settings_worker_shutdown_timeout(),
	Type = conductor_settings_worker_type(),
	CallbackModule = conductor_settings_worker_callback_module(),
	{Id, Start, Restart, ShutdownTimeout, Type, CallbackModule}.

conductor_settings_worker_id() ->
	conductor_settings_worker.
conductor_settings_worker_start() ->
	StartModule = conductor_settings_worker_start_module(),
	StartFunction = conductor_settings_worker_start_function(),
	StartArguments = conductor_settings_worker_start_arguments(),
	{StartModule, StartFunction, StartArguments}.

conductor_settings_worker_start_module() ->
	conductor_settings.
conductor_settings_worker_start_function() ->
	start_link.
conductor_settings_worker_start_arguments() ->
	[].

conductor_settings_worker_restart() ->
	permanent. %% Always restart child
conductor_settings_worker_shutdown_timeout() ->
	brutal_kill.
conductor_settings_worker_type() ->
	worker.
conductor_settings_worker_callback_module() ->
	[conductor_settings].

%% Conductor cache worker 
conductor_cache_worker_child() ->
	Id = conductor_cache_worker_id(),
	Start = conductor_cache_worker_start(),
	Restart = conductor_cache_worker_restart(),
	ShutdownTimeout = conductor_cache_worker_shutdown_timeout(),
	Type = conductor_cache_worker_type(),
	CallbackModule = conductor_cache_worker_callback_module(),
	{Id, Start, Restart, ShutdownTimeout, Type, CallbackModule}.

conductor_cache_worker_id() ->
	conductor_cache_worker.
conductor_cache_worker_start() ->
	StartModule = conductor_cache_worker_start_module(),
	StartFunction = conductor_cache_worker_start_function(),
	StartArguments = conductor_cache_worker_start_arguments(),
	{StartModule, StartFunction, StartArguments}.

conductor_cache_worker_start_module() ->
	conductor_cache.
conductor_cache_worker_start_function() ->
	start_link.
conductor_cache_worker_start_arguments() ->
	[].

conductor_cache_worker_restart() ->
	permanent. %% Always restart child
conductor_cache_worker_shutdown_timeout() ->
	brutal_kill.
conductor_cache_worker_type() ->
	worker.
conductor_cache_worker_callback_module() ->
	[conductor_cache].

%% Conductor response worker 
conductor_response_worker_child() ->
	Id = conductor_response_worker_id(),
	Start = conductor_response_worker_start(),
	Restart = conductor_response_worker_restart(),
	ShutdownTimeout = conductor_response_worker_shutdown_timeout(),
	Type = conductor_response_worker_type(),
	CallbackModule = conductor_response_worker_callback_module(),
	{Id, Start, Restart, ShutdownTimeout, Type, CallbackModule}.

conductor_response_worker_id() ->
	conductor_response_worker.
conductor_response_worker_start() ->
	StartModule = conductor_response_worker_start_module(),
	StartFunction = conductor_response_worker_start_function(),
	StartArguments = conductor_response_worker_start_arguments(),
	{StartModule, StartFunction, StartArguments}.

conductor_response_worker_start_module() ->
	conductor_response.
conductor_response_worker_start_function() ->
	start_link.
conductor_response_worker_start_arguments() ->
	[].

conductor_response_worker_restart() ->
	permanent. %% Always restart child
conductor_response_worker_shutdown_timeout() ->
	brutal_kill.
conductor_response_worker_type() ->
	worker.
conductor_response_worker_callback_module() ->
	[conductor_response].


%% ----------------------------------------------------------------------------
% Module functions
%% ----------------------------------------------------------------------------

% @doc Start supervisor if it is not already running
% @spec
start_link() ->
	Result = start_link_supervisor(),
	handle_supervisor_start(Result).

	start_link_supervisor() ->
	Name = supervisor_name_registration(),
	Module = supervisor_module(),
	InitArguments = supervisor_init_arguments(),
	supervisor:start_link(Name, Module, InitArguments).

supervisor_name_registration() ->
	{local, conductor_supervisor}.
supervisor_module() ->
	conductor_supervisor.
supervisor_init_arguments() ->
	[].

handle_supervisor_start({ok, Pid}) ->
	log_info("Conductor supervisor started", Pid),
	{ok, Pid};
handle_supervisor_start(ignore) ->
	log_info("Conductor supervisor returned ignore"),
	ignore;
handle_supervisor_start({error, Error}) ->
	log_error("ERROR: Can not start Conductor supervisor", Error),
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



