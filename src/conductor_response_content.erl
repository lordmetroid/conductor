-module(conductor_response_body).

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

	add_content/2,
	purge_content/1,
	get_content/1
]).

%% ----------------------------------------------------------------------------
% @doc Initialize a new response body
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
% Undefined content (Default)
%% ----------------------------------------------------------------------------
undefined(create_file, _From, _State) ->
	%% Create a file response body, FilePath
	{reply, ok, file, []};

undefined(create_program, _From, _State) ->
	%% Create a program response body, Content
	{reply, ok, program, []};

undefined(_Event, _From, State) ->
	{reply, error, undefined, State}.

%% ----------------------------------------------------------------------------
% File content
%% ----------------------------------------------------------------------------
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

file({add_content, FilePath}, _From, Content) ->
	%% Add binary content
	case file:read_file(FilePath) of
		{ok, Binary} ->
			%% File content 
			{reply, ok, file, Binary};
		{error, Reason} ->
			%% No file
			{reply, {error, Reason}, file, Content}
	end;

file(get_content, _From, Content) ->
	{reply, Content, file, Content};

file(destroy_body, _From, Content) ->
	%% Destroy response body
	{stop, normal, ok, Content};

file(_Event, _From, Content) ->
	{reply, error, file, Content}.

%% ----------------------------------------------------------------------------
% Program content
%% ----------------------------------------------------------------------------
program({set_mime_type, NewMimeType}, _From, {StatusCode,_MimeType}) ->
	%% Set program mime type
	{reply, ok, program, {StatusCode,NewMimeType}};

program(get_mime_type, _From, {StatusCode,MimeType}) ->
	%% Get program mime type
	{reply, MimeType, program, {StatusCode,MimeType}};

program({add_content, NewContent}, _From, Content) ->
	%% Add new content to program response
	{reply, ok, program, [NewContent | Content]};

program(purge_content, _From, _Content) ->
	%% Purge content
	{reply, ok, program, []};

program(get_content, _From, Content) ->
	%% Get and reset content
	{reply, lists:reverse(Content), program, Content};

program(destroy_body, _From, Content) ->
	%% Destroy response body
	{stop, normal, ok, Content};

program(_Event, _From, Content) ->
	{reply, error, program, Content}.

%% ----------------------------------------------------------------------------
% Response body control functions
%% ----------------------------------------------------------------------------
create_file() ->
	{ok, Body} = gen_fsm:start(?MODULE, [], []),
	gen_fsm:sync_send_event(Body, create_file),
	Body.

create_program() ->
	{ok, Body} = gen_fsm:start(?MODULE, [], []),
	gen_fsm:sync_send_event(Body, create_program),
	Body.

destroy(Body) ->
	gen_fsm:sync_send_event(Body, destroy_body).

set_mime_type(Header, NewMimeType) ->
	gen_fsm:sync_send_event(Header, {set_mime_type, NewMimeType}).

get_mime_type(Header) ->
	gen_fsm:sync_send_event(Header, get_mime_type).
%% ----------------------------------------------------------------------------
% Response body content function
%% ----------------------------------------------------------------------------
add_content(Body, NewContent) ->
	gen_fsm:sync_send_event(Body, {add_content, NewContent}).

purge_content(Body) ->
	gen_fsm:sync_send_event(Body, purge_content).

get_content(Body) ->
	gen_fsm:sync_send_event(Body, get_content).
