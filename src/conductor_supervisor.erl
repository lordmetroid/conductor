-module(conductor_supervisor).

-behavior(supervisor).
-export([
	init/1
]).

-export([
	start/0
]).

%% ----------------------------------------------------------------------------
% Supervisor callbacks
%% ----------------------------------------------------------------------------

% @doc Initializes a supervisor 
% @spec
init(_Arguments) ->
	initialize_supervisor().

initialize_supervisor() ->
	{
		create_supervisor_settings(),
		create_child_specifications()
	}.

create_supervisor_settings()
	{
		set_restart_strategy(),
		set_max_restarts(),
		set_max_restarts_reset_timer()
	}.
	
set_restart_strategy() ->
	one_for_one. %% Only restart the crashed child

set_max_restarts() ->
	10.

set_max_restarts_reset_timer() ->
	3600. %% Seconds

create_child_specificaions() ->
	[
		create_conductor_systems_supervisor_child(),
		create_web_interface_supervisor_child()
	].

create_conductor_systems_supervisor_child() ->

create_web_interface_supervisor_child() ->



			%% Conductor
			{conductor_system_supervisor,
				{?MODULE, start_conductor, []},
				permanent, infinity, supervisor, [?MODULE]
			},
			%% Webmachine
			{conductor_server_supervisor,
				{?MODULE, start_server, []},
			permanent, infinity, supervisor, [?MODULE]
			}

%% ----------------------------------------------------------------------------
% Module functions
%% ----------------------------------------------------------------------------

% @doc Start supervisor if it is not already running
% @spec
start() ->
	Result = start_supervisor(),
	handle_supervisor_start(Result).

start_supervisor() ->
	Registration = create_supervisor_local_name_registration_argument(),
	Module = create_supervisor_module_argument(),
	InitArguments = create_supervisor_init_arguments(),
	supervisor:start_link(Registration, Module, InitArguments).

create_supervisor_local_name_registration_argument() ->
	{local, conductor_supervisor}.

create_supervisor_module_argument() ->
	conductor_supervisor.

create_supervisor_init_arguments() ->
	[].

handle_supervisor_start({ok, Pid}) ->
	log_info(conductor_supervisor, {started, Pid}),
	{ok, Pid};
handle_supervisor_start(ignore) ->
	log_info(conductor_supervisor, ignore),
	ignore;
handle_supervisor_start({error, Error}) ->
	log_error(conductor_supervisor, Error),
	{error, Error}.

log_info(Operation, Information) ->
	lager: %% TODO

log_error(Opereation, Information)
	lager: %% TODO

	


%% ----------------------------------------------------------------------------
% @spec start_supervisors() ->
% @doc Start the application supervisors under a main supervisor
%% ----------------------------------------------------------------------------
start_supervisors() ->

%% ----------------------------------------------------------------------------
% @spec start_conductor() ->
% @doc Start the supervisor for the Conductor application processes
%% ----------------------------------------------------------------------------
start_conductor() ->
	supervisor:start_link({local, conductor_system_supervisor}, ?MODULE, {
		{one_for_one, 10, 3600}, [
			%% Conductor settings
			{conductor_settings,
				{conductor_settings, start_link, []},
				permanent, brutal_kill, worker, [conductor_settings]
			},
			%% Conductor cache
			{conductor_cache,
				{conductor_cache, start_link, []},
				permanent, brutal_kill, worker, [conductor_cache]
			},
			%% Conductor session manager
			{conductor_response,
				{conductor_response, start_link, []},
				permanent, brutal_kill, worker, [conductor_response]
			},
			%% Conductor logging manager
			{conductor_log,
				{conductor_log, start_link, []},
				permanent, brutal_kill, worker, [conductor_log]
			}
		]
	}).

%% ----------------------------------------------------------------------------
% @spec start_server->
% @doc Start the supervisor for Web-server
%% ----------------------------------------------------------------------------
start_server() ->
	%% Load the web-server settings
	ServerSettings = [
		%% Server settings
		{ip, conductor_settings:get(ip)},
		{port, conductor_settings:get(port)},
		{log_dir, conductor_settings:get(log_root)},

		%% Resource dispatcher
		{dispatch, [
			{['*'], conductor_dispatcher, []}
		]}
	],

	%% Start the web-server 
	supervisor:start_link({local, conductor_server_supervisor}, ?MODULE, {
		{one_for_one, 10, 3600}, [
			{webmachine_mochiweb,
				{webmachine_mochiweb, start, [ServerSettings]},
				permanent, 5000, worker, dynamic
			}
		]
	}).

