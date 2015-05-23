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
	
	add_data/1,
	get_data/0,
	purge_data/0
]).

%% ============================================================================
%% Callback functions
%% ============================================================================

init(_Arguments) ->
	%% Initalize an empty response manager
	{ok, []}.

handle_call({create_file, Request}, {Client, _}, Responses) ->
	{Result, DataId} = response_create_file(Request),

	case Result of
		error ->
			log_create_file_response_error(),
			{reply, error, Responses};
		ok ->
			{reply, ok, [{Client, Request, DataId} | Responses]}
	end;

handle_call({create_program, Request},  {Client, _}, Responses) ->
	{Result, DataId} = response_create_program(Request),

	case Result of
		error ->
			log_create_program_response_error(),
			{reply, error, Responses};
		ok ->
			{reply, Result, [{Client, Request, DataId} | Responses]}
	end;

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

handle_call({add_data, DataId}, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	Result = response_add_data(Response, DataId),
	{reply, Result, Responses};

handle_call(get_data, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	DataId = response_get_data(Response),
	{reply, DataId, Responses};

handle_call(purge_data, {Client, _}, Responses) ->
	Response = get_response(Client, Responses),
	Result = response_purge_data(Response),
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
	conductor_response_data:create_file(Request).

	
%% @doc Create a program response resource
%% @spec
create_program(Request) ->
	gen_server:call(?MODULE, {create_program, Request}).

response_create_program(Request) ->
	conductor_response_data:create_program(Request).


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
response_destroy(Responses, {Client, _Request, DataId}) ->
	case conductor_response_data:destroy(DataId) of
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
response_get_request({_Client, Request, _DataId}) ->
	Request.


%% @doc Set mime type of data
%% @spec
set_mime_type(MimeType) ->
	gen_server:call(?MODULE, {set_mime_type, MimeType}).

response_set_mime_type(false, _NewMimeType) ->
	error;	
response_set_mime_type({_Client, _Request, DataId}, NewMimeType) ->
	conductor_response_data:set_mime_type(DataId, NewMimeType).


%% @doc Get mime type of response data
%% @spec
get_mime_type() ->
	gen_server:call(?MODULE, get_mime_type).

response_get_mime_type(false) ->
	false;
response_get_mime_type({_Client, _Request, DataId}) ->
	conductor_response_header:get_mime_type(DataId).


%% @doc
%% @spec
add_data(DataId) ->
	gen_server:call(?MODULE, {add_data, DataId}).

response_add_data(false, _NewDataId) ->
	error;
response_add_data({_Client, _Request, DataId}, NewContent) ->
	conductor_response_body:add_data(DataId, NewContent).


%% @doc
%% @spec
get_data() ->
	gen_server:call(?MODULE, get_data).

response_get_data(false) ->
	false;
response_get_data({_Client, _Request, DataId}) ->
	conductor_response_body:get_data(DataId).


%% @doc
%% @spec
purge_data() ->
	gen_server:call(?MODULE, purge_data).

response_purge_data(false) ->
	false;
response_purge_data({_Client, _Request, DataId}) ->
	conductor_response_body:purge_data(DataId).

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

log_create_file_response_error() ->
	lager:warning("Could not create a file response").

log_create_program_response_error() ->
	lager:warning("Could not create a program response").

log_response_not_found_error() ->
    lager:warning("Could not find a matching response to the request").

