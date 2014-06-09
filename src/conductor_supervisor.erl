-module(conductor_supervisor).

-behavior(supervisor).
-export([
	init/1
]).

-export([
	start_link/0,
	start_supervisors/0,
	start_conductor/0,
	start_server/0
]).

%% ----------------------------------------------------------------------------
% @spec init(Arguments) -> {ok, Arguments}
% @doc Initialize a supervisor with the child specifications arguments
%% ----------------------------------------------------------------------------
init(Arguments) ->
	{ok, Arguments}.

%% ----------------------------------------------------------------------------
% @spec start_link() -> Result | {error, Reason}
% @doc Start the conductor application
%% ----------------------------------------------------------------------------
start_link() ->
	case whereis(?MODULE) of
		undefined ->
			?MODULE:start_supervisors();
		Pid ->
			{error, {already_started, Pid}}
	end.

%% ----------------------------------------------------------------------------
% @spec start_supervisors() -> 
% @doc Start the application supervisors under a main supervisor
%% ----------------------------------------------------------------------------
start_supervisors() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {
		{one_for_all, 10, 3600}, [
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
		]
	}).

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
			%% Conductor response manager
			{conductor_response,
				{conductor_response, start_link, []},
				permanent, brutal_kill, worker, [conductor_response]
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
		{log_root, conductor_settings:get(log_root)},

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
