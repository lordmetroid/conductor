-module(conductor_supervisor_settings).

-behaviour(supervisor).
-export([
	init/1
]).

-export([
	start_link/0
]).

init(_Arguments) ->

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [], []).
