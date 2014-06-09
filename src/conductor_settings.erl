-module(conductor_settings).

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
	get/1
]).

%% ----------------------------------------------------------------------------
% @doc Read all settings
%% ----------------------------------------------------------------------------
init(_Arguments) ->
	case init:get_argument(conf) of
		{ok, [Filename]} ->
			%% Get settings provided by a configuration file
			file:consult(Filename);
		error ->
			%% TODO: Use default settings
			{ok, []}
	end.

handle_call({get, Parameter}, _From, Settings) ->
	%% TODO: Check if file has been updated
	case lists:keyfind(Parameter, 1, Settings) of
		false ->
			{reply, undefined, Settings};
		{_, Value} ->
			{reply, Value, Settings}
	end;

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
% @doc Start the settings manager 
% -----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Parameter) ->
	gen_server:call(?MODULE, {get, Parameter}).
