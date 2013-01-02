-module (conductor_supervisor).

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
% @doc Initialize a supervisor
%% ----------------------------------------------------------------------------
init(Arguments) ->
	{ok, Arguments}.

%% ----------------------------------------------------------------------------
% @spec start_link() -> Result | {error, Reason}
% @doc Start Conductor
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
% @doc Start applications under a main supervisor
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
% @doc Start Conductor modules under separate supervisors
%% ----------------------------------------------------------------------------
start_conductor() ->
	supervisor:start_link({local, conductor_system_supervisor}, ?MODULE, {
		{one_for_one, 10, 3600}, [
			%% Settings manager
			{conductor_settings,
				{conductor_settings, start_link, []},
				permanent, brutal_kill, worker, [conductor_settings]
			}
			%% TODO Cache
		]
	}).

%% ----------------------------------------------------------------------------
% @spec start_server()-> 
% @doc Start Webmachine server 
%% ----------------------------------------------------------------------------
start_server() ->
	%% Initialize settings
	Settings = [
		{ip, conductor_settings:get(ip)},
		{port, conductor_settings:get(port)},
		{log_dir, conductor_settings:get(log_dir)},

		{dispatch, [
			{['*'], conductor_dispatcher, []}
		]}
	],

	%% Start the server
	supervisor:start_link({local, conductor_server_supervisor}, ?MODULE, {
		{one_for_one, 10, 3600}, [
			{webmachine_mochiweb,
				{webmachine_mochiweb, start, [Settings]},
				permanent, 5000, worker, dynamic
			}
		]
	}).

