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
	create_program/2,
	create_file/2,
	destroy/0,
	set_mime_type/2,
	get_mime_type/1,
	add_content/2,
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
file({add_content, FilePath}, _From, {Binary,FilePath}) ->
	%% TODO: Add binary content
	{reply, ok, file, {Binary,FilePath}};

file({replace_content, NewFilePath}, _From, {_Binary,_FilePath) ->
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
program({add_content, NewContent}, _From, Content) ->
	%% Add new content to program response
	{reply, ok, program, [NewContent | Content]};

program({replace_content, NewContent}, _From, _Content) ->
	%% Replace content
	{reply, ok, program, NewContent};

program(get_content, _From, Content) ->
	%% Get and reset content
	{reply, lists:reverse(Content), program, Content};

program(_Event, _From, Content) ->
	{reply, error, program, Content}.



%% ----------------------------------------------------------------------------
create_program(Body) ->
	gen_fsm:start(?MODULE, [], []),
	gen_fsm:sync_send_event(Body, create_program).

create_file(Body) ->
	gen_fsm:start(?MODULE, [], []),
	gen_fsm:sync_send_event(Body, create_file).

destroy() -> 
	gen_fsm:terminate

%% ----------------------------------------------------------------------------
add_content(Body, NewContent) ->
	gen_fsm:sync_send_event(Body, {add_content, NewContent}).

replace_content(Body, NewContet) ->
	gen_fsm:sync_send_event(Body, {replace_content, NewContent}).

get_content(Body) ->
	gen_fsm:sync_send_event(Body, get_content).
