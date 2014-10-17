-module(conductor_application).

-behavior(application).
-export([
	start/2,
	stop/1
]).

%% ----------------------------------------------------------------------------
% Application callbacks
%% ----------------------------------------------------------------------------

%% @doc Start Conductor web application platform application
start(_Type, _StartArguments) ->
	%% Start all application dependencies
	Applications = [kernel, stdlib, sasl, crypto, inets, mochiweb, webmachine],
	case start_applications(Applications) of
		{error, Errors} ->
			%% Could not start depending application
			{error, Errors};
		ok ->
			%% Start Conductor web application platform application
			conductor_supervisor:start_link()
	end.
	
%% @doc Stop Conductor web application platform application
stop(_State) ->
	stop.


%% ----------------------------------------------------------------------------
% Internal Functions
%% ----------------------------------------------------------------------------
%% @doc Start dependent applications if they are not already started
start_applications([]) ->
	%% All applications started
	ok;
start_applications([Application | Rest]) ->
	case application:start(Application) of
		{error, {already_started, Application}} ->
			%% Application already started
			start_applications(Rest);
		{error, Errors} ->
			%% Could not start application
			{error, Errors};
		ok ->
			%% Application started
			start_applications(Rest)
	end.
