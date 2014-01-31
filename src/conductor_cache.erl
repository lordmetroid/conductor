-module(conductor_cache).

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
	get_program/1,
	get_view/1
]).

%% ----------------------------------------------------------------------------
% @doc Compile and cache all programs and views
%% ----------------------------------------------------------------------------
init(_Arguments) ->

handle_call({get_program, Parameter}, _From, Cache) ->

handle_call({get_view, Parameter}, _From, Cache) ->

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