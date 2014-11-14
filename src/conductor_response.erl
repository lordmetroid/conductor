-module(conductor_response).

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
	%% Initalize an empty response manager
	{ok, []}.

%% ----------------------------------------------------------------------------
% Response control functions
%% ----------------------------------------------------------------------------
handle_call({create_file, Request}, {Client,_}, Responses) ->
	%% Create a file response
	Header = conductor_response_header:create_file(),
	Body =  conductor_response_body:create_file(),
	
	%% Add the response to the manager
	{reply, Client, [{Client, Request, {Header,Body}} | Responses]};

handle_call({create_program, Request},  {Client,_}, Responses) ->
	%% Create a program response
	Header = conductor_response_header:create_program(),
	Body =  conductor_response_body:create_program(),
	
	%% Add the response to the manager
	{reply, Client, [{Client, Request, {Header,Body}} | Responses]};

handle_call(terminate, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% response does not exist
			%% TODO: Write to log
			{reply, error, Responses};
		{Client, _Request, {Header,Body}} ->
			%% Destroy response
			conductor_response_header:destroy(Header),
			conductor_response_body:destroy(Body),

			%% Remove response from manager
			UpdatedResponses = lists:keydelete(Client,1, Responses),
			{reply, ok, UpdatedResponses}
	end;
%% ----------------------------------------------------------------------------
% Request control functions
%% ----------------------------------------------------------------------------
handle_call(get_request, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			%% TODO: Write to log
			{reply, error, Responses};			
		{Client, Request, {_Header,_Body}} ->
			{reply, Request, Responses}
	end;

%% ----------------------------------------------------------------------------
% Response header functions
%% ----------------------------------------------------------------------------
handle_call({set_status_code, NewStatusCode}, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			%% TODO: Write to log
			{reply, error, Responses};
		{Client, _Request, {Header,_Body}} ->
			%% Set new Status Code
			conductor_response_header:set_status_code(Header, NewStatusCode),
			{reply, ok, Responses}
	end;

handle_call(get_status_code, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {Header,_Body}} ->
			%% Get current status code
			StatusCode = conductor_response_header:get_status_code(Header),
			{reply, StatusCode, Responses}
	end;

handle_call({set_mime_type, NewMimeType}, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {Header,_Body}} ->
			conductor_response_header:set_mime_type(Header, NewMimeType),
			{reply, ok, Responses}
	end;

handle_call(get_mime_type, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {Header,_Body}} ->
			MimeType = conductor_response_header:get_mime_type(Header),
			{reply, MimeType, Responses}
	end;

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
handle_call({add_content, Content}, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {_Header,Body}} ->
			%% Add content to response body
			case conductor_response_body:add_content(Body, Content) of
				{error, Errors} ->
					%% TODO: Write error reason to log
					{reply, error, Responses};
				ok ->
					%% Content added
					{reply, ok, Responses}
			end
	end;

handle_call(purge_content, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {_Header,Body}} ->
			%% Purge all content from response body
			conductor_response_body:purge_content(Body),
			{reply, ok, Responses}
	end;

handle_call(get_content, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {_Header,Body}} ->
			%% Get current response body content
			Content = conductor_response_body:get_content(Body),
			{reply, Content, Responses}
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
% @doc Start the response manager 
% -----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
% Response control functions
%% ----------------------------------------------------------------------------
create_file(Request) ->
	gen_server:call(?MODULE, {create_file, Request}).

create_program(Request) ->
	gen_server:call(?MODULE, {create_program, Request}).

terminate() ->
	gen_server:call(?MODULE, terminate).

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

