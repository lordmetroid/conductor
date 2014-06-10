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
	start_session/0,
	end_session/0,
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

handle_call(start_session, From, Sessions) ->
	%% Start new session

	%% Start a new content state
	{ok, ContentId} =  conductor_content:init(),

	%% Add session to the manager
	{reply, From, [{From, {200, ContentId, []}} | Sessions]};

handle_call(end_session, From, Sessions) ->
	case lists:keyfind(From, 1, Sessions) of
		false ->
			%% Session does not exist
			{reply, ok, Sessions};
		{_StatusCode, ContentId, Log} ->
			%% Delete content
			conductor_content:delete(Content),

			%% Terminate session
			UpdatedSessions = lists:keydelete(From, Sessions),
			{reply, ok, UpdatedSessions}
	end;

handle_call({set_status_code, NewStatusCode}, From, Sessions) ->
	case lists:keyfind(From, 1, Sessions) of
		false ->
			%% Session does not exist
			{reply, ok, Sessions};
		{_StatusCode, ContentId, Log} ->
			%% Set new Status Code
			UpdatedSessions = lists:keyreplace(From, 1, Sessions, {
				NewStatusCode, Content, Log
			}),
			{reply, ok, UpdatedSessions}
	end;

handle_call(get_status_code, From, Sessions) ->
	case lists:keyfind(From, 1, Sessions) of
		false ->
			{reply, 
		{StatusCode, _ContentId, Log} ->
			{reply, StatusCode, Sessions}
	end;

handle_call(create_file, From, Sessions) ->

handle_call(create_program, From, Sessions) ->

handle_call({add_content, Content}, From, Sessions) ->




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
% Session functions
%% ----------------------------------------------------------------------------
start_session() ->
	gen_server:call(?MODULE, start_session).

end_session() ->
	gen_server:call(?MODULE, destroy_response).

set_status_code(Status) ->
	gen_server:call(?MODULE, {set_status_code, Status}).

get_status_code() ->
	gen_server:call(?MODULE, get_status_code).

%% ----------------------------------------------------------------------------
% Content functions
%% ----------------------------------------------------------------------------
create_file() ->
	gen_server:call(?MODULE, create_file).

create_program() ->
	gen_server:call(?MODULE, create_program).

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


