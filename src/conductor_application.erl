-module(conductor_application).

-behavior(application).
-export([
	start/2,
	stop/1
]).

%% ----------------------------------------------------------------------------
% @spec start(_, _) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
% @doc Start the Conductor web application platform
%% ----------------------------------------------------------------------------
start(_Type, _Arguments) ->
	case start_applications([sasl, crypto, inets, mochiweb, webmachine]) of
		ok ->
			conductor_supervisor:start_link();
		{error, Reason} ->
			{error, Reason}
	end.
%% ----------------------------------------------------------------------------
% @spec stop(_State) -> ok
% @doc Stop the Conductor web application platform
%% ----------------------------------------------------------------------------
stop(_State) ->
	ok.

%% ----------------------------------------------------------------------------
% @spec start_applications(Applications::[atom]) -> ok | {error, Reason}
% @doc Start specified applications
% @private
%% ----------------------------------------------------------------------------
start_applications([]) ->
	ok;
start_applications([Application | Rest]) ->
	case application:start(Application) of
		ok ->
			start_applications(Rest);
		{error, {already_started, Application}} ->
			start_applications(Rest);
		{error, Reason} ->
			{error, Reason}
	end.

