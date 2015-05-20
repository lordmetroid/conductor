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

	create_file/0,
	create_program/1,
	destroy/0,
	
	get_request/0,

	set_status_code/1,
	get_status_code/0,
	set_mime_type/1,
	get_mime_type/0,
	
	add_content/1,
	get_content/0,
	purge_content/0
]).

%% ============================================================================
%% Callback functions
%% ============================================================================

init(_Arguments) ->
	%% Initalize an empty response manager
	{ok, []}.

handle_call({create_file, Request}, {Client,_}, Responses) ->
	{Header, Body} = response_create_file();
	{reply, Client, [{Client, Request, {Header,Body}} | Responses]};

handle_call({create_program, Request},  {Client,_}, Responses) ->
	{Header, Body} = response_create_program()
	{reply, Client, [{Client, Request, {Header,Body}} | Responses]};

handle_call(destroy, {Client,_}, Responses) ->
	{Result, UpdatedResponse} = response_destroy(Client, Responses),
	{reply, Result, UpdatedResponses};

handle_call(get_request, {Client,_}, Responses) ->
	Request = response_get_request(Client, Responses),
	{reply, Request, Responses};

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

%% ============================================================================
%% Module functions
%% ============================================================================

%% @doc Start the response manager 
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ============================================================================
%% @doc
%% @spec
create_file(Request) ->
	gen_server:call(?MODULE, {create_file, Request}).

response_create_file() ->
	Header = conductor_response_header:create_file(),
	Body =  conductor_response_body:create_file(),
	{Header, Body}.
	
%% ============================================================================
%% @doc
%% @spec
create_program(Request) ->
	gen_server:call(?MODULE, {create_program, Request}).

response_create_program() ->
	Header = conductor_response_header:create_program(),
	Body =  conductor_response_body:create_program(),
	{Header, Body}.

%% ============================================================================
%% @doc
%% @spec
destroy() ->
	gen_server:call(?MODULE, destroy).

response_destroy(Client, Responses) ->
	case lists:keyfind(Client, 1, Responses) of
		false ->
			{{error, response_not_found}, Responses};
		{Client, _Request, {Header,Body}} ->
			conductor_response_header:destroy(Header),
			conductor_response_body:destroy(Body),

			UpdatedResponses = lists:keydelete(Client, 1, Responses),
			{ok, UpdatedResponses}
	end.

%% ============================================================================
%% @doc
%% @spec
get_request() ->
	gen_server:call(?MODULE, get_request).
	
response_get_request(Client, Responses) ->
	case lists:keyfind(Client, 1, Responses) of
		false ->
			false;
		{Client, Request, {_Header,_Body}} ->
			Request
	end.

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

%% ============================================================================
%% Logging functions
%% ============================================================================

