-module(conductor).
-compile({parse_transform, lager_transform}).

-behavior(application).
-export([
	start/2,
	stop/1
]).

%% ============================================================================
%  Application callback functions
%% ============================================================================

% @doc Starts the application
start(_Type, _StartArguments) ->
	conductor_supervisor:start_link().


% @doc Stops the application
stop(_State) ->
	stop.

