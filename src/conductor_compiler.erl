-module(conductor_compiler).

-export([
	compile/0
]).

%% ----------------------------------------------------------------------------
% @spec compile() -> 
% @doc Compile half an erlang file into a random module
%% ----------------------------------------------------------------------------
compile() ->

	erl_syntax:attribute(erl_syntax:atom(module), [
		%% Module id
		erl_syntax:atom()
	]).

