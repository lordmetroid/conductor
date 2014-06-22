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
	create_program/0,
	destroy/0,
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
% Response control functions
%% ----------------------------------------------------------------------------
handle_call(create_file, {Client,_}, Responses) ->
	%% Create a file response
	{ok, Header} = conductor_response_header:create_file(),
	{ok, Body} =  conductor_response_body:create_file(),
	
	%% Add the response to the manager
	{reply, Client, [{Client, {Header,Body}} | Responses]};

handle_call(create_program, {Client,_}, Responses) ->
	%% Create a program response
	{ok, Header} = conductor_response_header:create_program(),
	{ok, Body} =  conductor_response_body:create_program(),
	
	%% Add the response to the manager
	{reply, Client, [{Client, {Header,Body}} | Responses]};

handle_call(destroy, {Client,_}, Responses) ->
	case lists:keyfind(Client, 1, Responses) of
		false ->
			%% Response does not exist
			%% TODO: Write to log
			{reply, error, Responses};
		{Client, {Header,Body}} ->
			%% Destroy response
			conductor_response_header:destroy(Header),
			conductor_response_body:destroy(Body),

			%% Remove response from manager
			UpdatedResponses = lists:keydelete(Client, Responses),
			{reply, ok, UpdatedResponses}
	end;

%% ----------------------------------------------------------------------------
% Response header functions
%% ----------------------------------------------------------------------------
handle_call({set_status_code, NewStatusCode}, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Session does not exist
			%% TODO: Write to log
			{reply, error, Responses};
		{Client, {Header,_Body}} ->
			%% Set new Status Code
			conductor_response_header:set_status_code(Header, NewStatusCode),
			{reply, ok, Responses}
	end;

handle_call(get_status_code, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Session does not exist
			{reply, error, Responses};
		{Client, {Header,_Body}} ->
			%% Get current status code
			StatusCode = conductor_response_header:get_status_code(Header),
			{reply, StatusCode, Responses}
	end;

handle_call({set_mime_type, NewMimeType}, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Session does not exist
			{reply, error, Responses};
		{Client, {Header,_Body}} ->
			conductor_response_header:set_mime_type(Header, NewMimeType),
			{reply, ok, Responses}
	end;

handle_call(get_mime_type, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Session does not exist
			{reply, error, Responses};
		{Client, {Header,_Body}} ->
			MimeType = conductor_response_header:get_mime_type(Header),
			{reply, MimeType, Responses}
	end;

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
handle_call({add_content, Content}, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Session does not exist
			{reply, error, Responses};
		{Client, {_Header,Body}} ->
			%% Add content to response body
			case conductor_response_body:add_content(Body, Content) of
				{error, Reason} ->
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
			%% Session does not exist
			{reply, error, Responses};
		{Client, {_Header,Body}} ->
			%% Purge all content from response body
			conductor_response_body:purge_content(Body),
			{reply, ok, Responses}
	end;

handle_call({get_content}, {Client,_}, Responses) ->
	case lists:keyfind(Client,1, Responses) of
		false ->
			%% Session does not exist
			{reply, error, Responses};
		{Client, {_Header,Body}} ->
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
	gen_server:call(?MODULE, {set_mime_type, MimeType}).

get_mime_type() ->
	gen_server:call(?MODULE, get_mime_tyoe).

%% ----------------------------------------------------------------------------
% Response body functions
%% ----------------------------------------------------------------------------
add_content(Content) ->
	gen_server:call(?MODULE, {add_content, Content}).

purge_content() ->
	gen_server:call(?MODULE, purge_content).

get_content() ->
	gen_server:call(?MODULE, get_content).

