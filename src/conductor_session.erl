-module(conductor_session).

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

	create_file/1,
	create_program/1,
	destroy/0,
	
	get_request/0,

	set_status_code/1,
	get_status_code/0,
	set_mime_type/1,
	get_mime_type/0,
	add_content/1,
	purge_content/0,
	get_content/0
]).
init(_Arguments) ->
	%% Initalize an empty session manager
	{ok, []}.

%% ----------------------------------------------------------------------------
% Session control functions
%% ----------------------------------------------------------------------------
handle_call({create_file, Request}, {Client,_}, Sessions) ->
	%% Create a file session
	Header = conductor_response_header:create_file(),
	Body =  conductor_response_body:create_file(),
	
	%% Add the session to the manager
	{reply, Client, [{Client, Request, {Header,Body}} | Sessions]};

handle_call({create_program, Request},  {Client,_}, Sessions) ->
	%% Create a program session
	Header = conductor_response_header:create_program(),
	Body =  conductor_response_body:create_program(),
	
	%% Add the session to the manager
	{reply, Client, [{Client, Request, {Header,Body}} | Sessions]};

handle_call(destroy_session, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			%% TODO: Write to log
			{reply, error, Sessions};
		{Client, _Request, {Header,Body}} ->
			%% Destroy response
			conductor_response_header:destroy(Header),
			conductor_response_body:destroy(Body),

			%% Remove session from manager
			UpdatedSessions = lists:keydelete(Client,1, Sessions),
			{reply, ok, UpdatedSessions}
	end;
%% ----------------------------------------------------------------------------
% Request control functions
%% ----------------------------------------------------------------------------
handle_call(get_request, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			%% TODO: Write to log
			{reply, error, Sessions};			
		{Client, Request, {_Header,_Body}} ->
			{reply, Request, Sessions}
	end;

%% ----------------------------------------------------------------------------
% Response header functions
%% ----------------------------------------------------------------------------
handle_call({set_status_code, NewStatusCode}, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			%% TODO: Write to log
			{reply, error, Sessions};
		{Client, _Request, {Header,_Body}} ->
			%% Set new Status Code
			conductor_response_header:set_status_code(Header, NewStatusCode),
			{reply, ok, Sessions}
	end;

handle_call(get_status_code, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			{reply, error, Sessions};
		{Client, _Request, {Header,_Body}} ->
			%% Get current status code
			StatusCode = conductor_response_header:get_status_code(Header),
			{reply, StatusCode, Sessions}
	end;

handle_call({set_mime_type, NewMimeType}, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			{reply, error, Sessions};
		{Client, _Request, {Header,_Body}} ->
			conductor_response_header:set_mime_type(Header, NewMimeType),
			{reply, ok, Sessions}
	end;

handle_call(get_mime_type, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			{reply, error, Sessions};
		{Client, _Request, {Header,_Body}} ->
			MimeType = conductor_response_header:get_mime_type(Header),
			{reply, MimeType, Sessions}
	end;

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
handle_call({add_content, Content}, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			{reply, error, Sessions};
		{Client, _Request, {_Header,Body}} ->
			%% Add content to response body
			case conductor_response_body:add_content(Body, Content) of
				{error, Errors} ->
					%% TODO: Write error reason to log
					{reply, error, Sessions};
				ok ->
					%% Content added
					{reply, ok, Sessions}
			end
	end;

handle_call(purge_content, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			{reply, error, Sessions};
		{Client, _Request, {_Header,Body}} ->
			%% Purge all content from session body
			conductor_response_body:purge_content(Body),
			{reply, ok, Sessions}
	end;

handle_call(get_content, {Client,_}, Sessions) ->
	case lists:keyfind(Client,1, Sessions) of
		false ->
			%% Session does not exist
			{reply, error, Sessions};
		{Client, _Request, {_Header,Body}} ->
			%% Get current session body content
			Content = conductor_response_body:get_content(Body),
			{reply, Content, Sessions}
	end;


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

%% ----------------------------------------------------------------------------
% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
% @doc Start the session manager 
% -----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
% Session control functions
%% ----------------------------------------------------------------------------
create_file(Request) ->
	gen_server:call(?MODULE, {create_file, Request}).

create_program(Request) ->
	gen_server:call(?MODULE, {create_program, Request}).

destroy() ->
	gen_server:call(?MODULE, destroy_session).

%% ----------------------------------------------------------------------------
% Request control functions
%% ----------------------------------------------------------------------------
get_request() ->
	gen_server:call(?MODULE, get_request).
	
%% ----------------------------------------------------------------------------
% Response header functions
%% ----------------------------------------------------------------------------
set_status_code(Status) ->
	gen_server:call(?MODULE, {set_status_code, Status}).

get_status_code() ->
	gen_server:call(?MODULE, get_status_code).

set_mime_type(MimeType) ->
	gen_server:call(?MODULE, {set_mime_type, MimeType}).

get_mime_type() ->
	gen_server:call(?MODULE, get_mime_type).

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
add_content(Content) ->
	gen_server:call(?MODULE, {add_content, Content}).

purge_content() ->
	gen_server:call(?MODULE, purge_content).

get_content() ->
	gen_server:call(?MODULE, get_content).

