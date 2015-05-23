-module(conductor_response_data).

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

%% ============================================================================
%% Undefined data (Default)
%% ============================================================================
undefined(create_file, _From, _State) ->
	{reply, ok, file, []};

undefined(create_program, _From, _State) ->
	{reply, ok, program, []};

undefined(_Event, _From, State) ->
	{reply, error, undefined, State}.

%% ============================================================================
%% File data
%% ============================================================================
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

file({add_data, FilePath}, _From, Data) ->
	%% Add binary data
	case file:read_file(FilePath) of
		{ok, Binary} ->
			%% File data 
			{reply, ok, file, Binary};
		{error, Reason} ->
			%% No file
			{reply, {error, Reason}, file, Data}
	end;

file(get_data, _From, Data) ->
	{reply, Data, file, Content};

file(destroy, _From, Data) ->
	{stop, normal, ok, Data};

file(_Event, _From, Data) ->
	{reply, error, file, Data}.

%% ----------------------------------------------------------------------------
% Program data
%% ----------------------------------------------------------------------------
program({set_mime_type, NewMimeType}, _From, {StatusCode,_MimeType}) ->
	%% Set program mime type
	{reply, ok, program, {StatusCode,NewMimeType}};

program(get_mime_type, _From, {StatusCode,MimeType}) ->
	%% Get program mime type
	{reply, MimeType, program, {StatusCode,MimeType}};

program({add_data, NewData}, _From, Content) ->
	%% Add new data to program response
	{reply, ok, program, [NewData | Content]};

program(purge_data, _From, _Data) ->
	%% Purge data
	{reply, ok, program, []};

program(get_data, _From, Data) ->
	%% Get and reset data
	{reply, lists:reverse(Data), program, Content};

program(destroy_body, _From, Data) ->
	%% Destroy response body
	{stop, normal, ok, Data};

program(_Event, _From, Data) ->
	{reply, error, program, Data}.

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
% Response body data function
%% ----------------------------------------------------------------------------
add_data(Body, NewData) ->
	gen_fsm:sync_send_event(Body, {add_data, NewData}).

purge_data(Body) ->
	gen_fsm:sync_send_event(Body, purge_data).

get_data(Body) ->
	gen_fsm:sync_send_event(Body, get_data).
