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
]).
init(_Arguments) ->
	%% Initalize the session manager
	{ok, []}.

handle_call(create_file, From, Sessions) ->

handle_call(create_program, From, Sessions) ->

handle_call({set_status_code, Status}, From, Sessions) ->

handle_call(get_status_code, From, Sessions) ->

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

create_file_response() ->
	gen_server:call(?MODULE, create_file).

create_program_response() ->
	gen_server:call(?MODULE, create_program).

set_status_code(Status) ->
	gen_server:call(?MODULE, {set_status_code, Status}).

get_status_code() ->
	gen_server:call(?MODULE, get_status_code

add_content(Content) ->
	gen_server:call(?MODULE, {add_content, Content}).


