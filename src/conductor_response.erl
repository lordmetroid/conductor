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

%% ----------------------------------------------------------------------------
% Response control functions
%% ----------------------------------------------------------------------------
handle_call(create_file, From, Responses) ->
	%% Create a file response
	{ok, Header} = conductor_response_header:create_file(),
	{ok, Body} =  conductor_response_body:create_file(),
	{ok, Log} = ok, %% TODO: conductor_response_log:create(),
	
	%% Add the response to the manager
	{reply, From, [{From, {Header,Body, Log}} | Responses]};

handle_call(create_program, From, Response) ->
	%% Create a program response
	{ok, Header} = conductor_response_header:create_program(),
	{ok, Body} =  conductor_response_body:create_program(),
	{ok, Log} = ok, %% TODO: conductor_response_log:create(),
	
	%% Add the response to the manager
	{reply, From, [{From, {Header,Body, Log}} | Responses]};

handle_call(destroy, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Response does not exist
			%% TODO: Write to log
			{reply, ok, Responses};
		{Header,Body, Log} ->
			%% Destroy response
			conductor_response_header:destroy(Header),
			conductor_response_body:destroy(Body),
			%% TODO: conductor_response_log:destroy(Log),

			%% Remove response from manager
			UpdatedResponses = lists:keydelete(From, Responses),
			{reply, ok, UpdatedResponses}
	end;

%% ----------------------------------------------------------------------------
% Response header functions
%% ----------------------------------------------------------------------------
handle_call({set_status_code, NewStatusCode}, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Session does not exist
			%% TODO: Write to log
			{reply, false, Responses};
		{Header,_Body, Log} ->
			%% Set new Status Code
			UpdatedResponses = lists:keyreplace(From, 1, Responses, {
				NewStatusCode, Content, Log
			}),
			{reply, ok, UpdatedResponses}
	end;

handle_call(get_status_code, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Session does not exist
			{reply, false, Responses};
		{Header,Body, Log} ->
			{reply, StatusCode, Responses}
	end;

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
handle_call({add_content, Content}, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Session does not exist
			{reply, false, Responses};
		{Header,Body, Log} ->

	end;
handle_call({replace_content, Content}, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Session does not exist
			{reply, false, Responses};
		{Header,Body, Log} ->

	end;	

handle_call({get_content}, From, Responses) ->
	case lists:keyfind(From, 1, Responses) of
		false ->
			%% Session does not exist
			{reply, false, Responses};
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
% Response control functions
%% ----------------------------------------------------------------------------
create_file() ->
	gen_server:call(?MODULE, create_file).

create_program() ->
	gen_server:call(?MODULE, create_program).

destroy() ->
	gen_server:call(?MODULE, destroy_response).

%% ----------------------------------------------------------------------------
% Response header functions
%% ----------------------------------------------------------------------------
set_status_code(Status) ->
	gen_server:call(?MODULE, {set_status_code, Status}).

get_status_code() ->
	gen_server:call(?MODULE, get_status_code).

set_mime_type(MimeType) ->
	gen_server:call(?MODULE, {add_mime_type, MimeType}).

get_mime_type() ->
	gen_server:call(?MODULE, get_mime_tyoe).

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
add_content(Content) ->
	gen_server:call(?MODULE, {add_content, Content}).

replace_content(Content) ->
	gen_server:call(?MODULE, {replace_content, Content}).

get_content() ->
	gen_server:call(?MODULE, get_content).
