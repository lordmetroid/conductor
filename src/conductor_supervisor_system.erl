-module(conductor_supervisor_system).

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
% @spec init(Arguments) -> {ok, Arguments}
% @doc Initialize a supervisor with the child specifications arguments
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	{ok, {one_for_all, 10, 3600}, [
		%% Conductor settings manager
		{conductor_settings,
			{conductor_settings, start_link, []},
			permanent, brutal_kill, worker, [conductor_settings]
		},
		%% Conductor logging manager
		{conductor_log,
			{conductor_log, start_link, []},
			permanent, brutal_kill, worker, [conductor_log]
		},
		%% Conductor caching manager
		{conductor_cache,
			{conductor_cache, start_link, []},
			permanent, brutal_kill, worker, [conductor_cache]
		},
		%% Conductor response manager
		{conductor_response,
			{conductor_response, start_link, []},
			permanent, brutal_kill, worker, [conductor_response]
		}
	]}.

%% ----------------------------------------------------------------------------
% API
%% ----------------------------------------------------------------------------
start_link() ->
	case whereis(?MODULE) of
		undefined ->
			supervisor:start_link({local, ?MODULE}, ?MODULE, [], []);
		Pid ->
			{error, {already_started, Pid}}
	end.

