-module(conductor_web_interface_supervisor).

-behaviour(supervisor).
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

	{ok, {one_for_one, 10, 3600}, [
		{webmachine_mochiweb,
			{webmachine_mochiweb, start, [ServerSettings]},
			permanent, 5000, worker, dynamic
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

