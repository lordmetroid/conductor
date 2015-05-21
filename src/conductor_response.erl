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
	
	request/0,

	set_mime_type/1,
	mime_type/0,
	
	add_content/1,
	content/0,
	purge_content/0
]).

%% ============================================================================
%% Callback functions
%% ============================================================================

init(_Arguments) ->
	%% Initalize an empty response manager
	{ok, []}.

handle_call({create_file, Request}, {Client,_}, Responses) ->
	{Result, Content} = response_create_file();
	{reply, Result, [{Client, Request, Content}} | Responses]};

handle_call({create_program, Request},  {Client,_}, Responses) ->
	{Result, Content} = response_create_program()
	{reply, Result, [{Client, Request, Content} | Responses]};

handle_call(destroy, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	{Result, UpdatedResponse} = response_destroy(Responses, Response),
	{reply, Result, UpdatedResponses};

handle_call(request, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	Request = response_request(Response),
	{reply, Request, Responses};

handle_call(exists, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	Exists = response_exists(Response),
	{reply, Exists, Responses};

handle_call({set_mime_type, NewMimeType}, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	Result = response_set_mime_type(Response, NewMimeType),
	{reply, Result, Responses};

handle_call(mime_type, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	MimeType = response_mime_type(Responese),
	{reply, MimeType, Responses};

handle_call({add_content, Content}, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	Result = response_add_content(Response, Content),
	{reply, Result, Respones};

handle_call(content, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	Content = response_content(Response),
	{reply, Content, Responses};

handle_call(purge_content, {Client,_}, Responses) ->
	Response = get_response(Client, Responses),
	Result = response_purge_content(Response),
	{reply, Result, Responses};

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
%% Response control function
%% ============================================================================

%% @doc Create a file response resource
%% @spec
create_file(Request) ->
	gen_server:call(?MODULE, {create_file, Request}).

response_create_file(Request) ->
	case conductor_response_content:create_file(Request) of
		error ->
			error
		ok ->
			Content.

	
%% @doc Create a program response resource
%% @spec
create_program(Request) ->
	gen_server:call(?MODULE, {create_program, Request}).

response_create_program() ->
	Content = conductor_response_content:create_program(Request),
	Content.


%% @doc Destroy a response resource
%% @spec
destroy() ->
	gen_server:call(?MODULE, destroy).

response_destroy(Responses, false) ->
	{error, Responses};
response_destroy(Responses, {Client, _Request, Content}) ->
	case conductor_response_content:destroy(Content) of
		error ->
			{error, Responses};
		ok ->
			UpdatedResponses = lists:keydelete(Client, 1, Responses),
			{ok, UpdatedResponses}
	end.

%% ============================================================================
%% Response status function
%% ============================================================================

%% @doc Get the mathcing request to a response
%% @spec
request() ->
	gen_server:call(?MODULE, request).
	
response_request(Client, Responses) ->
	case lists:keyfind(Client, 1, Responses) of
		false ->
			false;
		{Client, Request, {_Header,_Body}} ->
			Request
	end.

%% @doc Check if response exists
%% @spec
exists() ->
	gen_server:call(?MODULE, exists).

response_exists(Client, Responses) ->
	case lists:keyfind(Client, 1, Responses) of
		false ->
			false;
		{Client, _Request, {_Header,_Body}} ->
			true
	end.

set_mime_type(MimeType) ->
	gen_server:call(?MODULE, {set_mime_type, MimeType}).

response_set_mime_type(Client, Responses, MimeType) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
		{Client, _Request, {Header,_Body}} ->
			conductor_response_header:set_mime_type(Header, NewMimeType),
	end;

%% @doc Get mime type of response content
%% @spec
mime_type() ->
	gen_server:call(?MODULE, mime_type).

response:mime_type(Client, Respones) ->
	case lists:keyfind(Client, 1, Responses) of
		false ->
			false;
		{Client, _Request, {Header,_Body}} ->
			MimeType = conductor_response_header:mime_type(Header),
	end;

%% ============================================================================
%% Response content functions
%% ============================================================================

add_content(Content) ->
	gen_server:call(?MODULE, {add_content, Content}).

response_add_content(Content) ->
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


get_content() ->
	gen_server:call(?MODULE, get_content).

	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {_Header,Body}} ->
			%% Get current response body content
			Content = conductor_response_body:get_content(Body),
			{reply, Content, Responses}
	end;

purge_content() ->
	gen_server:call(?MODULE, purge_content).
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Response does not exist
			{reply, error, Responses};
		{Client, _Request, {_Header,Body}} ->
			%% Purge all content from response body
			conductor_response_body:purge_content(Body),
			{reply, ok, Responses}
	end;

%% ============================================================================
%% Helper functions
%% ============================================================================

%% @doc Get the response which matches the client
get_response(Client, Responses) ->
	case lists:keyfind(Client, 1, Respones) of
		false ->
			log_response_not_found_error()
			false;
		Response ->
			Response
	end.

%% ============================================================================
%% Logging functions
%% ============================================================================

log_response_not_found_error() ->
    lager:warning("Could not find a matching response to the request").

