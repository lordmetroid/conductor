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
			%% Check if configuration file exist
			case filelib:last_modified(Filename) of
				0 ->
					%% Configuration file not found
					{ok, {[], [], []}};
				Date ->
					%% Get settings provided by a configuration file
					case file:consult(Filename) of
						{error, Errors} ->
							%% Could not interpret configuration file
							{ok, {[], [], []}};
						{ok, Settings} ->
							%% Return settings and configuration file
							{ok, {Settings, Filename, Date}}
					end
			end;
		error ->
			%% -conf Filename was not specified at runtime
			{ok, {[], [], []}}
	end.

handle_call({get, Parameter}, _From, {Settings, Filename, Date}) ->
	%% Check if file has been updated
	case filelib:last_modified(Filename) of
		0 ->
			%% File not found, continue to use old settings
			Value = get_value(Settings, Parameter),
			{reply, Value, {Settings, Filename, Date}};			
		Date ->
			%% Configuration file is not changed, get value from settings
			Value = get_value(Settings, Parameter),
			{reply, Value, {Settings, Filename, Date}};
		NewDate ->
			%% Configuration file has been updated
			case file:consult(Filename) of
				{error, Errors} ->
					%% Could not interpret updated configuration file
					conductor_log:add(Filename, "Could not be interpreted"),
					
					%% Use old settings until configuration file is corrected
					Value = get_value(Settings, Parameter),
					{reply, Value, {Settings, Filename, NewDate}};		
				{ok, UpdatedSettings} ->
					%% Get value from updated configuration file
					Value = get_value(UpdatedSettings, Parameter),
					{reply, Value, {UpdatedSettings, Filename, Date}}
			end
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
% @spec get_value() -> Value | undefined
% @doc Get value specified in settings for parameter
% -----------------------------------------------------------------------------
get_value(Settings, Parameter) ->
	case lists:keyfind(Parameter,1, Settings) of
		false ->
			%% Parameter not found
			undefined;
		{Parameter, Value} ->
			%% Return value
			Value
	end.

%% ----------------------------------------------------------------------------
% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
% @doc Start the settings manager 
% -----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Parameter) ->
	gen_server:call(?MODULE, {get, Parameter}).
