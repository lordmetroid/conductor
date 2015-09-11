-module(conductor_statistics).
-compile({parse_transform, lager_transform}).

-export([
	log/1
]).

-include_lib("webmachine/include/webmachine.hrl").

%% ============================================================================
%% Module functions
%% ============================================================================
log(Request) ->
	wrq

