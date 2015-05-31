-module(conductor_response_content).
-compile({parse_transform, lager_transform}).

-behavior(gen_fsm).
-export([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4,

	undefined/3,
	file/3,
	program/3
]).

-export([
	start/0,
	create_file/1,
	create_program/1,
	destroy/1,

	set_mime_type/2,
	get_mime_type/1,

	add_data/2,
	get_data/1,
	purge_data/1
]).

%% ============================================================================
%% Callback functions
%% ============================================================================

init(_Arguments) ->
	{ok, undefined, []}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, error, StateName, State}.

handle_info(_Information, StateName, State) ->
	{reply, error, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVersion, StateName, State, _Extra) ->
	{ok, StateName, State}.

%% ============================================================================
%% Undefined content (Default)
%% ============================================================================
undefined(create_file, _From, _State) ->
	{reply, ok, file, []};

undefined(create_program, _From, _State) ->
	{reply, ok, program, {"text/html", []}};

undefined(destroy, _From, State) ->
	{stop, normal, ok, State};

undefined(Event, _From, State) ->
	log_not_supported_by_undefined_error(Event),
	{reply, error, undefined, State}.

%% ============================================================================
%% File content 
%% ============================================================================

file(get_mime_type, _From, FilePath) ->
	MimeType = file_get_mime_type(FilePath),
	{reply, MimeType, file, FilePath};

file({add_data, NewFilePath}, _From, _FilePath) ->
	{reply, ok, file, NewFilePath};

file(get_data, _From, FilePath) ->
	Data = file_get_data(FilePath),
	{reply, Data, file, FilePath};

file(purge_data, _From, _FilePath) ->
	{reply, ok, file, []};

file(destroy, _From, FilePath) ->
	{stop, normal, ok, FilePath};

file(Event, _From, State) ->
	log_not_supported_by_file_error(Event),
	{reply, error, file, State}.

%% ============================================================================
%% Program content 
%% ============================================================================

program({set_mime_type, NewMimeType}, _From, {_MimeType, Data}) ->
	{reply, ok, program, {NewMimeType, Data}};

program(get_mime_type, _From, {MimeType, Data}) ->
	{reply, MimeType, program, {MimeType, Data}};

program({add_data, NewData}, _From, {MimeType, Data}) ->
	{reply, ok, program, {MimeType, [NewData | Data]}};

program(get_data, _From, {MimeType, Data}) ->
	{reply, Data ,program, {MimeType, Data}};

program(purge_data, _From, {MimeType, _Data}) ->
	{reply, ok, program, {MimeType, []}};

program(destroy, _From, {MimeType, Data}) ->
	{stop, normal, ok, {MimeType, Data}};

program(Event, _From, State) ->
	log_not_supported_by_program_error(Event),
	{reply, error, program, State}.

%% ============================================================================
%% Module functions
%% ============================================================================

%% @doc
%% @spec start() -> ignore | {error, Reason} | {ok, Content}
start() ->
	gen_fsm:start(?MODULE, [], []).

%% @doc
%% @spec
create_file(Content) ->
	gen_fsm:sync_send_event(Content, create_file).

%% @doc
%% @spec
create_program(Content) ->
	gen_fsm:sync_send_event(Content, create_program).

%% @doc
%% @spec
destroy(Content) ->
	gen_fsm:sync_send_event(Content, destroy).

%% @doc
%% @spec
set_mime_type(Content, NewMimeType) ->
	gen_fsm:sync_send_event(Content, {set_mime_type, NewMimeType}).


%% @doc Get the mime type of the data in the response
%% @spec
get_mime_type(Content) ->
	gen_fsm:sync_send_event(Content, get_mime_type).

file_get_mime_type(FilePath) ->
	case filelib:is_file(FilePath) of
		false ->
			log_read_file_error(FilePath, enoent),
			undefined;
		true ->
			[MimeType | _Rest] = mimetypes:filename(FilePath),
			binary_to_list(MimeType)
	end.

%% @doc Add data to response
%% @spec
add_data(Content, NewData) ->
	gen_fsm:sync_send_event(Content, {add_data, NewData}).

%% @doc Get the mime type of the data in the response
%% @spec
get_data(Content) ->
	gen_fsm:sync_send_event(Content, get_data).

file_get_data(FilePath) ->
	case file:read_file(FilePath) of
		{error, Reason} ->
			log_read_file_error(FilePath, Reason),
			[];
		{ok, Binary} ->
			Binary
	end.

%% @doc
%% @spec
purge_data(Content) ->
	gen_fsm:sync_send_event(Content, purge_data).


%% ============================================================================
%% Logging function
%% ============================================================================

log_not_supported_by_undefined_error(Event) ->
	lager:warning("Undefined response do not support ~p", [Event]).

log_not_supported_by_file_error(Event) ->
	lager:warning("File response do not support ~p", [Event]).

log_not_supported_by_program_error(Event) ->
	lager:warning("Program response do not support ~p", [Event]).

log_read_file_error(FilePath, Reason) ->
	lager:warning("Could not read ~s, ~p", [FilePath, Reason]).

