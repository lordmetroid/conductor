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
	exists/0,
	destroy/0,

	get_request/0,

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

handle_call({create_file, Request}, {Client, _}, Responses) ->
	{Result, Content} = response_create_file(Request),
	{reply, Result, [{Client, Request, Content} | Responses]};

handle_call({create_program, Request},  {Client, _}, Responses) ->
	{Result, Content} = response_create_program(Request),
	{reply, Result, [{Client, Request, Content} | Responses]};

handle_call(exists, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	Exists = response_exists(Response),
	{reply, Exists, Responses};

handle_call(destroy, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	{Result, UpdatedResponses} = response_destroy(Responses, Response),
	{reply, Result, UpdatedResponses};

handle_call(get_request, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	Request = response_get_request(Response),
	{reply, Request, Responses};

handle_call({set_mime_type, NewMimeType}, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	Result = response_set_mime_type(Response, NewMimeType),
	{reply, Result, Responses};

handle_call(get_mime_type, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	MimeType = response_get_mime_type(Response),
	{reply, MimeType, Responses};

handle_call({add_content, Content}, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	Result = response_add_content(Response, Content),
	{reply, Result, Responses};

handle_call(get_content, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	Content = response_get_content(Response),
	{reply, Content, Responses};

handle_call(purge_content, {Client, _}, Responses) ->
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

%% @doc Create a file response resource
%% @spec
create_file(Request) ->
	gen_server:call(?MODULE, {create_file, Request}).

response_create_file(Request) ->
	conductor_response_content:create_file(Request).
	
%% @doc Create a program response resource
%% @spec
create_program(Request) ->
	gen_server:call(?MODULE, {create_program, Request}).

response_create_program(Request) ->
	conductor_response_content:create_program(Request).

%% @doc Check if response exists
%% @spec
exists() ->
	gen_server:call(?MODULE, exists).

response_exists(false) ->
	false;
response_exists(_Response) ->
	true.

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
%% @doc Get the mathcing request to a response
%% @spec
get_request() ->
	gen_server:call(?MODULE, get_request).

response_get_request(false) ->
	false;
response_get_request({_Client, Request, _Content}) ->
	Request.

%% @doc Set mime type of content
%% @spec
set_mime_type(MimeType) ->
	gen_server:call(?MODULE, {set_mime_type, MimeType}).

response_set_mime_type(false, _NewMimeType) ->
	error;	
response_set_mime_type({_Client, _Request, Content}, NewMimeType) ->
	conductor_response_content:set_mime_type(Content, NewMimeType).

%% @doc Get mime type of response content
%% @spec
get_mime_type() ->
	gen_server:call(?MODULE, get_mime_type).

response_get_mime_type(false) ->
	false;
response_get_mime_type({_Client, _Request, Content}) ->
	conductor_response_header:get_mime_type(Content).

%% @doc
%% @spec
add_content(Content) ->
	gen_server:call(?MODULE, {add_content, Content}).

response_add_content(false, _NewContent) ->
	error;
response_add_content({_Client, _Request, Content}, NewContent) ->
	conductor_response_body:add_content(Content, NewContent).

%% @doc
%% @spec
get_content() ->
	gen_server:call(?MODULE, get_content).

response_get_content(false) ->
	false;
response_get_content({_Client, _Request, Content}) ->
	conductor_response_body:get_content(Content).

%% @doc
%% @spec
purge_content() ->
	gen_server:call(?MODULE, purge_content).

response_purge_content(false) ->
	false;
response_purge_content({_Client, _Request, Content}) ->
	conductor_response_body:purge_content(Content).

%% ============================================================================
%% Helper functions
%% ============================================================================

%% @doc Get the response which matches the client
get_response(Client, Responses) ->
	case lists:keyfind(Client, 1, Responses) of
		false ->
			log_response_not_found_error(),
			false;
		Response ->
			Response
	end.

%% ============================================================================
%% Logging functions
%% ============================================================================

log_response_not_found_error() ->
    lager:warning("Could not find a matching response to the request").

