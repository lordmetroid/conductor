-module(conductor_application).

-behavior(application).
-export([
	start/2,
	stop/1
]).

%% ----------------------------------------------------------------------------
% Application callbacks
%% ----------------------------------------------------------------------------

% @doc Starts the application
% @spec
start(_Type, _StartArguments) ->
	log_info(conductor_application, starting),
	start_application_supervisor().

start_application_supervisor() ->
	conductor_supervisor:start().

% @doc Stops the application
% @spec
stop(_State) ->
	log_info(application, stopping),
	stop_application().

stop_application() ->
	stop.

log_info(Operation, Information) ->
	lager:

