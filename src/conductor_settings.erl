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

%% @doc Initial loading of all configurations
init(_Arguments) ->
	ConfigDirectory = get_config_directory(),
	FileNames = get_config_files(ConfigDirectory),

	case read_config_files(FileNames, []) of
		[] ->
			log_no_configurations_error(ConfigDirectory),
			{ok, []};
		Settings ->
			{ok, Settings}
	end.

get_config_directory() ->
	{ok, [[DirectoryName]]} = init:get_argument(conf),
	DirectoryName.

get_config_files(ConfigDirectory) ->
	case file:list_dir_all(ConfigDirectory) of
		{error, Reason} ->
			log_directory_error(ConfigDirectory, Reason);
		{ok, FileNames} ->
			FileNames	
	end.

read_config_files([], Settings) ->
	Settings;
read_config_files([FileName | Rest], Settings) ->
	Configurations = file:consult(FileName),
	Date = filelib:last_modified(FileName),

	NewSettings = create_settings(FileName, Date, Configurations),
	read_config_files(Rest, NewSettings ++ Settings).

create_settings(FileName, 0, _Configurations) ->
	log_file_not_found_error(FileName),
	[];
create_settings(FileName, _Date, {error, Reason}) ->
	log_file_interpret_error(FileName, Reason),
	[];
create_settings(FileName, Date, {ok, [{Domain, Configurations}]}) ->
	[{Domain, Configurations, FileName, Date}].



%% ============================================================================
%% @doc Get a settings value
%% @spec
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

log_no_configurations_error(DirectoryName) ->
	lager:warning("Could not find configuration files in ~s", [DirectoryName]).

log_directory_error(ConfigDirectory, Reason) ->
	lager:waning("Could not access configuration files: ~p ~s", [Reason, ConfigDirectory]).

log_file_not_found_error(FileName) ->
	lager:warning("Could not find configuration file: ~s", [FileName]).

log_file_interpret_error(FileName, Reason) when is_atom(Reason) ->
	lager:warning("Could not interpret ~s: ~s", [FileName, Reason]);
log_file_interpret_error(FileName, Reason) ->
	Error = file:format_error(Reason),
	lager:warning("Could not interpret ~s: ~s", [FileName, Error]).
