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
	add_file/1,
	get/2
]).

%% ============================================================================
%  Generic Server callback functions
%% ============================================================================

%% @doc Initial loading of all configurations
init(_Arguments) ->
	settings_init().

handle_call({get, Domain, Parameter}, _From, Settings) ->
	setting_get(Domain, Parameter, Settings);

handle_call({add_file, FilePath}, _From, Settings) ->
	settings_add_file(FilePath, Settings);

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
	get_config_directory().

get_config_directory() ->
	case init:get_argument(conf) of
		error ->
			log_conf_not_set_error(),
			{ok, []};
		{ok, [[DirectoryPath]]} ->
			check_conf_is_directory(DirectoryPath)
	end.

check_conf_is_directory(DirectoryPath) ->
	case filelib:is_dir(DirectoryPath) of
		false ->
			log_conf_not_directory_error(DirectoryPath),
			{ok, []};
		true ->
			get_config_files(DirectoryPath)
	end.

get_config_files(DirectoryPath) ->
	case file:list_dir_all(DirectoryPath) of
		{error, Reason} ->
			log_directory_error(DirectoryPath, Reason),
			{ok, []};
		{ok, FileNames} ->
			read_config_files(DirectoryPath, FileNames, [])
	end.

read_config_files(DirectoryPath, [], []) ->
	log_no_configurations_error(DirectoryPath),
	{ok, {DirectoryPath, []}};
read_config_files(DirectoryPath, [], Settings) ->
	{ok, {DirectoryPath, Settings}};
read_config_files(DirectoryPath, [FileName | Rest], Settings) ->
	FilePath = filename:join(DirectoryPath, FileName),

	Configurations = file:consult(FilePath),
	Date = filelib:last_modified(FilePath),

	NewSettings = create_settings(FilePath, Date, Configurations),
	read_config_files(DirectoryPath, Rest, NewSettings ++ Settings).


create_settings(FilePath, 0, _Configurations) ->
	log_file_not_found_error(FilePath),
	[];
create_settings(FilePath, _Date, {error, Reason}) ->
	log_file_interpret_error(FilePath, Reason),
	[];
create_settings(FilePath, Date, {ok, [{Domain, Configurations}]}) ->
	[{Domain, Configurations, FilePath, Date}].

%% @doc Hot plug a configuration file
add_file(FilePath) ->
	gen_server:call(?MODULE, {add_file, FilePath}).

%% @doc Get a settings parameter
get(Domain, Parameter) ->
	gen_server:call(?MODULE, {get, Parameter}).

%% ============================================================================
%% @doc
%% @spec
settings_get(Domain, Parameter, {DirectoryPath, Settings}) ->
  	case lists:keyfind(Domain, 1, Settings) of
		false ->

		 Configurations ->
			get_config_updates(Paramter, Configurations, DirectoryPath)
	end.

get_config_updates(Parameter, Configurations, DirectoryPath) ->
	case filelib:last_modified(FilePath) of
		0 ->
		Date ->
			
		NewDate ->
	end.





%% ============================================================================
%  Logging functions
%% ============================================================================
log_conf_not_set_error() ->
	lager:warning("-conf $CONFIG_DIRECTORY not specified").

log_conf_not_directory_error(DirectoryPath) ->
	lager:warning("-conf ~s does not specify a directory", [DirectoryPath]).

log_no_configurations_error(DirectoryPath) ->
	lager:warning("Found no configuration files in ~s", [DirectoryPath]).

log_directory_error(DirectoryPath, Reason) ->
	lager:waning("Could not access ~s: ~s", [DirectoryPath, Reason]).

log_file_not_found_error(FilePath) ->
	lager:warning("Could not find configuration file: ~s", [FilePath]).

log_file_interpret_error(_FilePath, Reason) when is_atom(Reason) ->
	no_log;
log_file_interpret_error(FilePath, Reason) ->
	Error = file:format_error(Reason),
	lager:warning("Could not interpret ~s: ~s", [FilePath, Error]).
