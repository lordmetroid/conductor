-module(conductor_application).

-behavior(application).
-export([
	start/2,
	stop/1
]).

%% ----------------------------------------------------------------------------
% @spec start(_Type, _Arguments) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
% @doc Launch the application supervisor tree
%% ----------------------------------------------------------------------------
start(_Type, _Arguments) ->
	conductor_supervisor:start_link().


%% ----------------------------------------------------------------------------
% @spec stop(_State) -> ok
% @doc Stop the application
%% ----------------------------------------------------------------------------
stop(_State) ->
	ok.


