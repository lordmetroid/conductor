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
start(_Type, _Arguments) ->
	case init:get_argument(conf) of
		error ->
			log_conf_not_set_error(),
			{error, conf_not_set};
		{ok, [[DirectoryName]]} ->
			check_conf_is_directory(DirectoryName)
	end.

check_conf_is_directory(DirectoryName) ->
	case filelib:is_dir(DirectoryName) of
		false ->
			log_conf_not_directory_error(DirectoryName),
			{error, conf_not_dir};
		true ->
			conductor_supervisor:start_link()
	end.


% @doc Stops the application
stop(_State) ->
	stop.

%% ============================================================================
%% Logging function
%% ============================================================================
log_conf_not_set_error() ->
	lager:error("Could not start Conductor: -conf $CONFIG_DIR not specified").

log_conf_not_directory_error(DirectoryName) ->
	lager:error("Could not start Conductor: -conf ~s does not specify a directory", [DirectoryName]).


