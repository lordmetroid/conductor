-module(conductor_settings).
-compile({parse_transform, lager_transform}).

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

%% ============================================================================
%  Generic Server callback functions
%% ============================================================================
init(_Arguments) ->
	case init:get_argument(conf) of
		error ->
			log_conf_not_set_error(),
			init:stop();
		{ok, [[DirectoryName]]} ->
			check_is_directory(DirectoryName)
	end.

check_is_directory(DirectoryName) ->
	case filelib:is_dir(DirectoryName) of
		false ->
			log_conf_not_set_error(),
			init:stop();
		true ->
			DirectoryName
	end.

get_config_files(ConfigDirectory) ->
	case 



check_config_file(FileName) ->
	case get_file_modified_date(FileName) of
		not_found ->
			log_file_not_found_error(FileName),
			init:stop();
		Date ->
			read_config_file(FileName, Date)
	end.

read_config_file(FileName, Date) ->
	case file:consult(FileName) of
		{error, Reason} ->
			log_file_interpret_error(Reason),
			init:stop();
		{ok, Settings} ->
			{ok, {Settings, Date, FileName}}
	end.


%% ============================================================================
%% @doc Get a settings value
%% @spec
handle_call({get, Parameter}, _From, {Settings, FileName, Date}) ->
	case 


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
					{reply, Value, {UpdatedSettings, Filename, NewDate}}
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



%% ============================================================================
%  Module functions
%% ============================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Parameter) ->
	gen_server:call(?MODULE, {get, Parameter}).

%% ============================================================================
%  Helper functions
%% ============================================================================

%% @doc
get_file_modified_date(FileName) ->
	case filelib:last_modified(FileName) of
		0 ->
			log_file_not_found_error(FileName),
			not_found;
		Date ->
			Date
	end.

get_value(Settings, Parameter) ->
	case lists:keyfind(Parameter,1, Settings) of
		false ->
			%% Parameter not found
			undefined;
		{Parameter, Value} ->
			%% Return value
			Value
	end.

%% ============================================================================
%  Logging functions
%% ============================================================================

log_conf_not_set_error() ->
	lager:error("Could not start Conductor: -conf $CONFIG_DIR not specified").

log_file_not_found_error(FileName) ->
	lager:warning("Could not find configuration file: ~s", [FileName]).

log_file_interpret_error(Reason) when is_atom(Reason) ->
	lager:error("Could not interpret configuration file: ~s", [Reason]);
log_file_interpret_error(Reason) ->
	Error = file:format_error(Reason),
	lager:error("Could not interpret configuration file: ~s", [Error]).
