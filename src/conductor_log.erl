-module(conductor_log).

-behavior(gen_server).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	start_link/0,
	
	add/2
]).
init(_Arguments) ->
	%% Initalize a logging manager
	LogRoot = conductor_settings:get(log_root),
	LogPath = filename:join([LogRoot, "conductor.log"]),
	
	{ok, LogPath}.

handle_call({add, Source, Message}, _From, LogPath) ->
	%% TODO: Add date
	file:write_file(LogPath, Source++ ":\n\t" ++Message++"\n", [append]),
	{reply, ok, LogPath};

handle_call(_Event, _From, State) ->
	{stop, State}.

handle_cast(_Event, State) ->
	{stop, State}.

handle_info(_Information, State) ->
	{stop, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% ----------------------------------------------------------------------------
% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
% @doc Start the response manager 
% -----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Source, Message) ->
	gen_server:call(?MODULE, {add, Source, Message}).
	
