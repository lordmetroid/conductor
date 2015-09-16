-module(conductor_statistics).
-compile({parse_transform, lager_transform}).

-export([
	log/3
]).

-include_lib("webmachine/include/webmachine.hrl").

%% ============================================================================
%% Module functions
%% ============================================================================
log(_Request, _Domain, _Path) ->
	ok.
