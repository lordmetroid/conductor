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
	log_info("Starting Conductor application"),
	start_application_dependencies(),
	start_application_supervisor().

start_application_dependencies() ->
	application:ensure_all_started(conductor).

start_application_supervisor() ->
	conductor_supervisor:start_link().


% @doc Stops the application
% @spec
stop(_State) ->
	log_info("Stopping Conductor application"),
	stop_application().

stop_application() ->
	stop.

%% ----------------------------------------------------------------------------
% Logging functions
%% ----------------------------------------------------------------------------

log_info(Message) ->
	lager:info(Message).

