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
	start/0,
	create_program/2,
	create_file/2,
	set_mime_type/2,
	get_mime_type/1,
	add_content/2,
	get_content/1
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
% Undefined content (Default)
%% ----------------------------------------------------------------------------
undefined(create_file, _From, _State) ->
	%% Create a file response, {Binary,Filename}
	{reply, ok, file, {[],[]}};

undefined(create_program, _From, _State) ->
	%% Create a program response {Body,MimeType}
	{reply, ok, program, {[],"text/html"}};

undefined(_Event, _From, State) ->
	{reply, error, undefined, State}.

%% ----------------------------------------------------------------------------
% File content
%% ----------------------------------------------------------------------------
file(get_mime_type, _From, {Binary,FilePath}) ->
	%% Get file mime type
	case mimetypes:filename(FilePath) of
		undefined ->
			{reply, "text/html", file, {Binary,FilePath}};
		MimeType ->
			{reply, MimeType, file,{Binary,FilePath}}
	end;

file({add_content, FilePath}, _From, {Binary,FilePath}) ->
	%% TODO: Add binary content
	{reply, ok, file, {Binary,FilePath}};

file({replace_content, NewFilePath}, _From, {_Binary,FilePath) ->
	%% TODO: add binary content
	{reply, ok, file, {NewBinary,NewFilePath}};
	
file(get_content, _From, {Binary,FilePath}) ->
	%% Get file binary
	{reply, Binary, file, {Binary,FilePath}};

file(_Event, _From, {Binary,FilePath}) ->
	{reply, error, file, {Binary,FilePath}}.

%% ----------------------------------------------------------------------------
% Program content
%% ----------------------------------------------------------------------------
program({set_mime_type, NewMimeType}, _From, {Content,_MimeType}) ->
	%% Set content mime type
	{reply, ok, program, {Content,NewMimeType}};

program(get_mime_type, _From, {Content,MimeType}) ->
	%% Get content mime type
	{reply, MimeType, program, {Content,MimeType}};

program({add_content, NewContent}, _From, {Content,MimeType}) ->
	%% Add new content to program response
	{reply, ok, program, {[NewContent | Content],MimeType}};

program({replace_content, NewContent}, _From, {_Content,MimeType}) ->
	%% Replace content
	{reply, ok, program, {NewContent,MimeType}};

program(get_content, _From, {Content,MimeType}) ->
	%% Get and reset content
	{reply, lists:reverse(Content), program, {Content,MimeType}};

program(_Event, _From, {Content,MimeType}) ->
	{reply, error, program, {Content,MimeType}}.


%% ----------------------------------------------------------------------------
%
%% ----------------------------------------------------------------------------
init() ->
	gen_fsm:start(?MODULE, [], []).

delete() -> 
	gen_fsm:terminate

create_program(ContentId) ->
	gen_fsm:sync_send_event(ContentId, create_program).

create_file(ContentId) ->
	gen_fsm:sync_send_event(ContentId, create_file).

set_mime_type(ContentId, NewMimeType) ->
	gen_fsm:sync_send_event(ContentId, {set_mime_type, NewMimeType}).

get_mime_type(ContentId) ->
	gen_fsm:sync_send_event(ContentId, get_mime_type).

add_content(ContentId, NewContent) ->
	gen_fsm:sync_send_event(ContentId, {add_content, NewContent}).

replace_content(ContentId, NewContet) ->
	gen_fsm:sync_send_event(ContentId, {replace_content, NewContent}).

get_content(ContentId) ->
	gen_fsm:sync_send_event(ContentId, get_content).

