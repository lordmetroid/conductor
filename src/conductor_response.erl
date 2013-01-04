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
	create/2,
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
undefined({create, file}, _From, _State) ->
	%% Create a file response
	{reply, ok, file, []};

undefined({create, program}, _From, _State) ->
	%% Create a program response
	{reply, ok, program, {[], "text/html"}};

undefined(_Event, _From, State) ->
	{reply, error, undefined, State}.

%% ----------------------------------------------------------------------------
% File response
%% ----------------------------------------------------------------------------
file(get_mime_type, _From, Filename) ->
	%% Get file mime type
	case mimetypes:filename(Filename) of
		undefined ->
			{reply, "text/html", file, Filename};
		MimeType ->
			{reply, MimeType, file, Filename}
	end;

file({add_content, NewFilename}, _From, _Filename) ->
	%%TODO: Add Get binary from cache
	{reply, ok, file, NewFilename};

file(get_content, _From, Filename) ->
	%% TODO: Return binary
	{reply, Filename, file, Filename};

file(_Event, _From, Filename) ->
	{reply, error, file, Filename}.

%% ----------------------------------------------------------------------------
% Program response
%% ----------------------------------------------------------------------------
program({set_mime_type, NewMimeType}, _From, {Content, _MimeType}) ->
	%% Set content mime type
	{reply, ok, program, {Content, NewMimeType}};

program(get_mime_type, _From, {Content, MimeType}) ->
	%% Get content mime type
	{reply, MimeType, program, {Content, MimeType}};

program({add_content, NewContent}, _From, {Content, MimeType}) ->
	%% Add new content to program response
	{reply, ok, program, {[NewContent | Content], MimeType}};

program(get_content, _From, {Content, MimeType}) ->
	%% Get content
	{reply, lists:reverse(Content), program, {Content, MimeType}};

program(_Event, _From, {Content, MimeType}) ->
	{reply, error, program, {Content, MimeType}}.


%% ----------------------------------------------------------------------------
%
%% ----------------------------------------------------------------------------
start() ->
	gen_fsm:start(?MODULE, [], []).

create(Response, Type) ->
	gen_fsm:sync_send_event(Response, {create, Type}).

set_mime_type(Response, NewMimeType) ->
	gen_fsm:sync_send_event(Response, {set_mime_type, NewMimeType}).

get_mime_type(Response) ->
	gen_fsm:sync_send_event(Response, get_mime_type).

add_content(Response, NewContent) ->
	gen_fsm:sync_send_event(Response, {add_content, NewContent}).

get_content(Response) ->
	gen_fsm:sync_send_event(Response, get_content).

