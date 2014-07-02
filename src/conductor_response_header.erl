-module(conductor_response_header).

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
	
	set_status_code/2,
	get_status_code/1,
	set_mime_type/2,
	get_mime_type/1
]).

%% ----------------------------------------------------------------------------
% @doc Start a new content container
%% ----------------------------------------------------------------------------
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

%% ----------------------------------------------------------------------------
% Undefined header type (Default)
%% ----------------------------------------------------------------------------
undefined(create_file, _From, _State) ->
	%% Create a file response header, {StatusCode,MimeType}
	{reply, ok, file, {200,[]}};

undefined(create_program, _From, _State) ->
	%% Create a program response header, {StatusCode,MimeType}
	{reply, ok, program, {200,"text/html"}};

undefined(_Event, _From, State) ->
	{reply, error, undefined, State}.

%% ----------------------------------------------------------------------------
% File header
%% ----------------------------------------------------------------------------
file({set_status_code, NewStatusCode}, _From, {_StatusCode,MimeType}) ->
	%% Set HTTP Status Code
	{reply, ok, file, {NewStatusCode,MimeType}};

file(get_status_code, _From, {StatusCode,MimeType}) ->
	%% Get HTTP Status Code
	{reply, StatusCode, file, {StatusCode,MimeType}};

file({set_mime_type, NewMimeType}, _From, {StatusCode,_MimeType}) ->
	%% Set file mime type
	{reply, ok, file, {StatusCode,NewMimeType}};
	
file(get_mime_type, _From, {StatusCode,MimeType}) ->
	case filelib:is_file(MimeType) of
		false ->
			%% Return set mime type
			{reply, MimeType, file, {StatusCode,MimeType}};
		true ->
			%% Guess mime type from filepath
			case mimetypes:filename(MimeType) of
				undefined ->
					%% Return default mime type
					{reply, "text/html", file, {StatusCode,MimeType}};
				MimeType ->
					%% Return mime type of file
					{reply, MimeType, file, {StatusCode,MimeType}}
			end
	end;

file(destroy_header, _From, {StatusCode,MimeType}) ->
	%% Destroy response
	{stop, ok, {StatusCode,MimeType}};

file(_Event, _From, {StatusCode,MimeType}) ->
	{reply, error, file, {StatusCode,MimeType}}.

%% ----------------------------------------------------------------------------
% Program header
%% ----------------------------------------------------------------------------
program({set_status_code, NewStatusCode}, _From, {_StatusCode,MimeType}) ->
	%% Set HTTP Status Code
	{reply, ok, file, {NewStatusCode,MimeType}};

program(get_status_code, _From, {StatusCode,MimeType}) ->
	%% Get HTTP Status Code
	{reply, StatusCode, file, {StatusCode,MimeType}};

program({set_mime_type, NewMimeType}, _From, {StatusCode,_MimeType}) ->
	%% Set program mime type
	{reply, ok, program, {StatusCode,NewMimeType}};

program(get_mime_type, _From, {StatusCode,MimeType}) ->
	%% Get program mime type
	{reply, MimeType, program, {StatusCode,MimeType}};

program(destroy_header, _From, {StatusCode,MimeType}) ->
	%% Destroy response
	{stop, ok, {StatusCode,MimeType}};

program(_Event, _From, {StatusCode,MimeType}) ->
	{reply, error, program, {StatusCode,MimeType}}.
	
%% ----------------------------------------------------------------------------
create_file() ->
	{ok, Header} = gen_fsm:start(?MODULE, [], []),
	gen_fsm:sync_send_event(Header, create_file),
	Header.

create_program() ->
	{ok, Header} = gen_fsm:start(?MODULE, [], []),
	gen_fsm:sync_send_event(Header, create_program),
	Header.

destroy(Header) -> 
	gen_fsm:sync_send_event(Header, destroy_header).
	
%% ----------------------------------------------------------------------------
set_status_code(Header, NewStatusCode) ->
	gen_fsm:sync_send_event(Header, {set_status_code, NewStatusCode}).

get_status_code(Header) ->
	gen_fsm:sync_send_event(Header, get_status_code).

set_mime_type(Header, NewMimeType) ->
	gen_fsm:sync_send_event(Header, {set_mime_type, NewMimeType}).

get_mime_type(Header) ->
	gen_fsm:sync_send_event(Header, get_mime_type).
