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
	set_directory/1,
	get/2
]).

%% ============================================================================
%  Generic Server callback functions
%% ============================================================================

%% @doc Initial loading of all configurations
init(_Arguments) ->
	Settings = settings_init(),
	{ok, Settings}.

handle_call({set_directory, Path}, _From, Settings) ->
	NewSettings = settings_set_directory(Path, Settings),
	{reply, ok, NewSettings};

handle_call({get, Domain, Argument}, _From, Settings) ->
	Value = settings_get(Domain, Argument, Settings),
	{reply, Value, Settings};

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

%% @doc Start the settings server and read the intial settings
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Read and use the initial settings 
%% @spec settings_init() -> {ok, Settings}
settings_init() ->
	case ets:info(condcutor_settings) of
		undefined ->
			ets:new(conductor_settings, [set, named_table]),
			get_conf_argument();
		_Exists ->
			get_conf_argument()
	end.


get_conf_argument() ->
	case ets:lookup(conductor_settings, conf) of
		[] ->
			add_conf_argument();
		[{conf, Path}] ->
			check_conf_is_directory(Path)
	end.
				
add_conf_argument() ->
	case init:get_argument(conf) of
		error ->
			log_conf_not_set_error(),
			[];
		{ok, [[Path]]} ->
			check_conf_is_directory(Path)
	end.

check_conf_is_directory(Path) ->
	case filelib:is_dir(Path) of
		false ->
			log_conf_not_directory_error(Path),
			[];		
		true ->
			get_config_files(Path)
	end.

get_config_files(Path) ->
	case file:list_dir_all(Path) of
		{error, Reason} ->
			log_directory_error(Path, Reason),
			[];
		{ok, FileNames} ->
			read_config_files(Path, FileNames, [])
	end.

read_config_files(Path, [], []) ->
	log_no_configurations_error(Path),
	[];
read_config_files(Path, [], Settings) ->
	ets:insert(conductor_settings, {conf, Path}),
	Settings;
read_config_files(Path, [FileName | Rest], Settings) ->
	FilePath = filename:join(Path, FileName),

	FileContent = file:consult(FilePath),
	Date = filelib:last_modified(FilePath),

	Setting = create_setting(FilePath, Date, FileContent),

	read_config_files(Path, Rest, Setting ++ Settings).



%% ============================================================================
%% @doc Set and use a new configuration directory
%% @spec set_directory(NewPath::string()) -> Settings
set_directory(Path) ->
	gen_server:call(?MODULE, {set_directory, Path}).

settings_set_directory(Path, Settings) ->
	case filelib:is_dir(Path) of
		false ->
			log_not_a_directory_error(Path),
			Settings;
		true ->
			get_new_config_files(Path, Settings)
	end.

get_new_config_files(Path, Settings) ->
	case file:list_dir_all(Path) of
		{error, Reason} ->
			log_directory_error(Path, Reason),
			Settings;
		{ok, FileNames} ->
			read_new_config_files(Path, FileNames, Settings, [])
	end.

read_new_config_files(Path, [], Settings, []) ->
	log_no_configurations_error(Path),
	Settings;
read_new_config_files(Path, [], _Settings, NewSettings) ->
	ets:insert(conductor_settings, {conf, Path}),
	NewSettings;
read_new_config_files(Path, [FileName | Rest], _Settings, NewSettings) ->
	FilePath = filename:join(Path, FileName),

	FileContent = file:consult(FilePath),
	Date = filelib:last_modified(FilePath),

	Setting = create_setting(FilePath, Date, FileContent),

	read_config_files(Path, Rest, Setting ++ NewSettings).



%% ============================================================================
%% @doc Get a settings parameter
%% @spec get(Domain::string(), Parameter::atom) -> Value::term() | undefined
get(Domain, Parameter) ->
	gen_server:call(?MODULE, {get, Domain, Parameter}).

settings_get(Domain, Parameter, {Path, Settings}) ->
	ok.

%% ============================================================================
%  Helper functions
%% ============================================================================

create_setting(FilePath, 0, _FileContent) ->
	log_file_not_found_error(FilePath),
	[];
create_setting(FilePath, _Date, {error, Reason}) ->
	log_file_interpret_error(FilePath, Reason),
	[];
create_setting(FilePath, Date, {ok, [{Domain, Values}]}) ->
	[{Domain, Values, FilePath, Date}].

%% ============================================================================
%  Logging functions
%% ============================================================================
log_conf_not_set_error() ->
	lager:warning("-conf $CONFIG_DIRECTORY not specified").

log_conf_not_directory_error(Path) ->
	lager:warning("-conf ~s does not specify a directory", [Path]).

log_not_a_directory_error(Path) ->
	lager:warning("~s does not specify a directory", [Path]).

log_no_configurations_error(Path) ->
	lager:warning("Found no valid configuration files in ~s", [Path]).

log_directory_error(Path, Reason) ->
	lager:waning("Could not access ~s: ~s", [Path, Reason]).

log_file_not_found_error(FilePath) ->
	lager:warning("Could not find configuration file: ~s", [FilePath]).

log_file_interpret_error(_FilePath, Reason) when is_atom(Reason) ->
	no_log;
log_file_interpret_error(FilePath, Reason) ->
	Error = file:format_error(Reason),
	lager:warning("Could not interpret ~s: ~s", [FilePath, Error]).

log_no_value(Parameter, FilePath) ->
	lager:warning("Parameter ~s is not defined in ~s", [Parameter, FilePath]).
	
