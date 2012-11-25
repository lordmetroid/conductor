-module(conductor).

-export([
	start/0,
	stop/0,
	restart/0
]).

%% ----------------------------------------------------------------------------
% @spec start() -> ok | {error, Reason}
% @doc Start the Conductor Web Application Platform
%% ----------------------------------------------------------------------------
start() ->
	%% Create a list of dependencies available in /deps
	{file, ModulePath} = code:is_loaded(?MODULE),
	Root = filename:dirname(filename:dirname(ModulePath)),
	Dependencies = filelib:wildcard(filename:join([Root, "deps","*","ebin"])),

	%% Load all dependencies in deps/
	code:add_paths(Dependencies),

	%% Start Conductor
	application:start(?MODULE).

%% ----------------------------------------------------------------------------
% @spec stop() -> ok | {error, Reason}
% @doc Stop the Conductor Web Application Platform
%% ----------------------------------------------------------------------------
stop() ->
	application:stop(?MODULE).

%% ----------------------------------------------------------------------------
% @spec restart() -> ok | {error, Reason}
% @doc Restart the Conductor Web Application Platform
%% ----------------------------------------------------------------------------
restart() ->
	case ?MODULE:stop() of
		ok ->
			start();
		{error, {not_started, ?MODULE}} ->
			start();
		{error, Reason} ->
			{error, Reason}
	end.
