-module(conductor_response_content).

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
	create_file/0,
	create_program/0,
	destroy/1,

	add_data/2,
	purge_data/1,
	get_data/1
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
	{reply, ok, file, {[],[]}};

undefined(create_program, _From, _State) ->
	{reply, ok, program, {[],[]}};

undefined(_Event, _From, State) ->
	{reply, error, undefined, State}.

%% ============================================================================
%% File content 
%% ============================================================================

file(set_mime_type, _From, FilePath) ->
	{reply, error, file, FilePath};

file(get_mime_type, _From, FilePath) ->
	MimeType = file_get_mime_type(FilePath),
	{reply, MimeType, file, FilePath};

file({add_data, NewFilePath}, _From, FilePath) ->
	{reply, Result, file, NewFilePath};

file(get_data, _From, FilePath) ->
	Data = file_get_data()
	{reply, Data, file, FilePath};

file(purge_data, _From, _FilePath) ->
	{reply, ok, file, []};

file(destroy, _From, Content) ->
	{stop, normal, ok, Data};

file(_Event, _From, Content) ->
	{reply, error, file, Data}.

%% ============================================================================
%% Program content 
%% ============================================================================

program({set_mime_type, NewMimeType}, _From, {StatusCode,_MimeType}) ->
	%% Set program mime type
	{reply, ok, program, {StatusCode,NewMimeType}};

program(get_mime_type, _From, {StatusCode,MimeType}) ->
	%% Get program mime type
	{reply, MimeType, program, {StatusCode,MimeType}};

program({add_data, NewData}, _From, Content) ->
	%% Add new data to program response
	{reply, ok, program, [NewContent | Content]};

program(get_data, _From, Content) ->
	%% Get and reset data
	{reply, ,program, Data};

program(purge_data, _From, _Content) ->
	%% Purge data
	{reply, ok, program, []};

program(destroy, _From, Content) ->
	%% Destroy response body
	{stop, normal, ok, Data};

program(_Event, _From, Content) ->
	{reply, error, program, Data}.

%% ============================================================================
%% Module functions
%% ============================================================================
create_file() ->
	case gen_fsm:start(?MODULE, [], []) of
		ignore ->
			log_create_response_error(ignore),
			{error, ignore};
		{error, Reason} ->
			log_create_response_error(Reason),
			{error, Reason};
		{ok, Data} ->
			gen_fsm:sync_send_event(Content, create_file),
			{ok, Data}
	end.

create_program() ->
	case gen_fsm:start(?MODULE, [], []) of
		ignore ->
			log_create_response_error(ignore),
			{error, ignore};
		{error, Reason} ->
			log_create_response_error(Reason),
			{error, Reason};
		{ok, Data} ->
			gen_fsm:sync_send_event(Content, create_program),
			{ok, Data}
	end.

destroy(Content) ->
	gen_fsm:sync_send_event(Content, destroy_body).



set_mime_type(Header, NewMimeType) ->
	gen_fsm:sync_send_event(Header, {set_mime_type, NewMimeType}).


%% @doc
%% @spec
get_mime_type(Content) ->
	gen_fsm:sync_send_event(Content, get_mime_type).

file_get_mime_type(FilePath) ->
	case filelib:is_file(Filepath) of
		false ->
			log_invalid_filepath_error(FilePath),
			"";
		true ->
			get_file_mime_type(FilePath)
	end.


get_file_mime_type(FilePath) ->
	case mimetypes:filename(MimeType) of
		undefined ->
			"text/html";
		MimeType ->
			MimeType
	end.



add_data(Body, NewContent) ->
	gen_fsm:sync_send_event(Body, {add_data, NewData}).

file_add_data(FilePath) ->
	case file:read_file(FilePath) of
		{ok, Binary} ->
			
		{error, Reason} ->
	end;

purge_data(Body) ->
	gen_fsm:sync_send_event(Body, purge_data).

get_data(Body) ->
	gen_fsm:sync_send_event(Body, get_data).

%% ============================================================================
%% Logging function
%% ============================================================================

log_invalid_file_error(FilePath) ->
	lager:warning("Invalid file, ~s", [FilePath]).

log_response_create_error(Reason) ->
	lager:warning("Could not create a response: ~s", [Reason]).


