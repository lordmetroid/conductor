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
	create/0,
	destroy/0,
	set_status_code/1,
	get_status_code/0,
	add_mime_type/1,
	get_mime_type/0,
	add_content/1,
	replace_content/1,
	get_content/0
]).
init(_Arguments) ->
	%% Initalize the session manager
	{ok, []}.

handle_call(start_session, From, Responses) ->
	%% Initialize a new response
	{ok, Header} = conductor_response_header:init(),
	{ok, Body} =  conductor_response_body:init(),
	{ok, Log} = conductor_response_log:init(),

	%% Add response to the manager
	{reply, From, [{From, {Header,Body, Log}} | Responses]};

handle_call(end_session, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Response does not exist
			{reply, ok, Responses};
		{_Header,Body, Log} ->
			%% Delete response body
			conductor_response_body:delete(Content),

			%% Destroy response
			UpdatedResponses = lists:keydelete(From, Responses),
			{reply, ok, UpdatedResponses}
	end;

handle_call({set_status_code, NewStatusCode}, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Session does not exist
			{reply, ok, Responses};
		{_Header,Body, Log} ->
			%% Set new Status Code
			UpdatedResponses = lists:keyreplace(From, 1, Responses, {
				NewStatusCode, Content, Log
			}),
			{reply, ok, UpdatedResponses}
	end;

handle_call(get_status_code, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			{reply, 
		{Header,Body, Log} ->
			{reply, StatusCode, Responses}
	end;

handle_call(create_file, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
		{Header,Body, Log} ->
	end;

handle_call(create_program, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
		{Header,Body, Log} ->
	end;
handle_call({add_content, Content}, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
		{Header,Body, Log} ->
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
% Response functions
%% ----------------------------------------------------------------------------
destroy() ->
	gen_server:call(?MODULE, destroy_response).

create_file() ->
	gen_server:call(?MODULE, create_file).

create_program() ->
	gen_server:call(?MODULE, create_program).

%% ----------------------------------------------------------------------------
% Response header functions
%% ----------------------------------------------------------------------------
set_status_code(Status) ->
	gen_server:call(?MODULE, {set_status_code, Status}).

get_status_code() ->
	gen_server:call(?MODULE, get_status_code).

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
add_content(Content) ->
	gen_server:call(?MODULE, {add_content, Content}).

replace_content(Content) ->
	gen_server:call(?MODULE, {replace_content, Content}).

get_content() ->
	gen_server:call(?MODULE, get_content).

add_mime_type(MimeType) ->
	gen_server:call(?MODULE, {add_mime_type, MimeType}).

get_mime_type() ->
	gen_server:call(?MODULE, get_mime_tyoe).
