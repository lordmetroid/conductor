-module(conductor_response).

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
% @doc Start a new response with the default undefined response type
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
% Undefined response (Default)
%% ----------------------------------------------------------------------------
undefined(create_file, _From, _State) ->
	%% Create a file response, {Binary,Filename}
	{reply, ok, file, {[],[]}};

undefined(create_program, _From, _State) ->
	%% Create a program response {Status,Content,MimeType}
	{reply, ok, program, {200,[],"text/html"}};

undefined(_Event, _From, State) ->
	{reply, error, undefined, State}.

%% ----------------------------------------------------------------------------
% File response
%% ----------------------------------------------------------------------------
file(get_status, _From, {Binary,FilePath}) ->
	%% Get HTTP Status Code
	{reply, 200, file, {Binary,FilePath}};

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

file(get_content, _From, {Binary,FilePath}) ->
	%% Get and resrt file binary
	{reply, Binary, file, {[], FilePath}};

file(_Event, _From, {Binary,FilePath}) ->
	{reply, error, file, {Binary,FilePath}}.

%% ----------------------------------------------------------------------------
% Program response
%% ----------------------------------------------------------------------------
program({set_status, NewStatus}, _From, {_Status,Content,MimeType}) ->
	%% Set HTTP Status Code
	{reply, ok, program, {NewStatus,Content,Mimetype}};

program(get_status, _From, {Status,Content,MimeType}) ->
	%% Get HTTP Status Code
	{reply, Status, program, {Status,Content,MimeType};

program({set_mime_type, NewMimeType}, _From, {Status,Content,_MimeType}) ->
	%% Set content mime type
	{reply, ok, program, {Status,Content,NewMimeType}};

program(get_mime_type, _From, {Status,Content,MimeType}) ->
	%% Get content mime type
	{reply, MimeType, program, {Status,Content,MimeType}};

program({add_content, NewContent}, _From, {Status,Content,MimeType}) ->
	%% Add new content to program response
	{reply, ok, program, {Status,[NewContent, Content],MimeType}};

program({replace_content, NewContent}, _From, {Status,_Content,MimeType}) ->
	%% Replace content
	{reply, ok, program, {Status,NewContent,MimeType}};

program(get_content, _From, {Status,Content,MimeType}) ->
	%% Get and reset content
	{reply, lists:reverse(Content), program, {Status,[],MimeType}};

program(_Event, _From, {Status,Content,MimeType}) ->
	{reply, error, program, {StatusCode,Content,MimeType}}.


%% ----------------------------------------------------------------------------
%
%% ----------------------------------------------------------------------------
start() ->
	gen_fsm:start(?MODULE, [], []).

create_program(Response) ->
	gen_fsm:sync_send_event(Response, create_program).

create_file(Response ->
	gen_fsm:sync_send_event(Response, create_file).

set_status(Response, Status) ->
	gen_fsm:sync_send_event(Response, {set_status, Status}).

get_status(Response) ->
	gen_fsm:sync_send_event(Response, get_status).

set_mime_type(Response, NewMimeType) ->
	gen_fsm:sync_send_event(Response, {set_mime_type, NewMimeType}).

get_mime_type(Response) ->
	gen_fsm:sync_send_event(Response, get_mime_type).

add_content(Response, NewContent) ->
	gen_fsm:sync_send_event(Response, {add_content, NewContent}).

replace_content(Response, NewContet) ->
	gen_fsm:sync_send_event(Response, {replace_content, NewContent}).

get_content(Response) ->
	gen_fsm:sync_send_event(Response, get_content).

