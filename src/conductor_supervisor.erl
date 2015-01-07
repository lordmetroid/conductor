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
% @spec init(Arguments) -> {ok, Arguments}
% @doc Initialize a supervisor with the child specifications arguments
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	{ok, {one_for_all, 10, 3600}, [
		%% Conductor systems supervisor
		{conductor_supervisor_system,
			{conductor_supervisor_system, start_conductor, []},
			permanent, infinity, supervisor, [conductor_supervisor_system]
		},
		%% Conductor interface server supervisor
		{conductor_supervisor_interface,
			{conductor_supervisor_interface, start_link, []},
			permanent, infinity, supervisor, [conductor_supervisor_interface]
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

