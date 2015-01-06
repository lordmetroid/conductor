-module(conductor_supervisor_log).

-behaviour(supervisor).
-export([
	init/1
]).

-export([
	start_link/0
]).

init(_Arguments) ->
	ok.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [], []).
