-module(conductor_compiler).

-export([
	make/1
]).



make(Paths) ->
	make(Paths, []).

make([], Modules) ->
	%% Return compiled modules
	Modules;
make([Path | Rest], Components) ->
	%% create a module from source file
	make(Rest, [make_module(Path) | Modules]).

make_module(Path) ->
	%% Add dynamic module id
	ModuleId = generate_module_id(),
	add_module_id(ModuleId).
	
	%% TODO: Identify module type

	%% TODO: Add controller standard helper function

%% ----------------------------------------------------------------------------
% Compilation helper functions
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec generate_module_id() -> UUID::string()
% @doc Generate a module id
%% ----------------------------------------------------------------------------
generate_module_id() ->
	uuid:uuid_to_string(uuid:get_v4()).

%% ----------------------------------------------------------------------------
% @spec add_module_id() -> erl_syntax()
% @doc Add module Erlang id header
%% ----------------------------------------------------------------------------
add_module_id(ModuleId) ->
	erl_syntax:attribute(erl_syntax:atom(module), [
		erl_syntax:atom(ModuleId)
	]).
	
add_model_call(Model, function, Arguments) ->
	ok.

add_view_call(View, Arguments) ->
	ok.
add_controller_call(Controller, function, Arguments) ->
	ok.
