-module(conductor_compiler).

-export([
	make_programs/1,
	make_program/1,
	make_models/1,
	make_model/1,
	make_views/1,
	make_view/1,
	make_controllers/1,
	make_controller/1
]).

%% TODO: Breakout the repeating steps from the different component compilations


%% ----------------------------------------------------------------------------
% @spec make_programs(Paths) -> {ModuleId, Erlang module)
% @doc Compile programs from provided paths
%% ----------------------------------------------------------------------------
make_programs(Paths) ->
	make_programs(Paths, []).

make_programs([], Programs) ->
	Programs;
make_programs([Path | Rest], Programs) ->
	make_programs(Rest, [make_program(Path) | Programs]).
	
make_program(Path) ->
	%% TODO: Add module ID
	ModuleId = generate_module_id(),
	add_module_id(ModuleId).
	
	%% TODO: Check if called models, views or controllers exists
	
	%% TODO: Add execute function header

%% ----------------------------------------------------------------------------
% @spec make_models(Paths) -> {ModuleId, Erlang module)
% @doc Compile models from provided paths
%% ----------------------------------------------------------------------------
make_models(Paths) ->
	make_models(Paths, []).

make_models([], Models) ->
	Models;
make_models([Path | Rest], Models) ->
	make_models(Rest, [make_model(Path) | Models]).

make_model(Path) ->
	%% TODO: Add module ID
	ModuleId = generate_module_id(),
	add_module_id(ModuleId).
	
%% ----------------------------------------------------------------------------
% @spec make_views(Paths) -> {ModuleId, Erlang module)
% @doc Compile views from provided paths
%% ----------------------------------------------------------------------------
make_views(Paths) ->
	make_views(Paths, []).

make_views([], Views) ->
	Views;
make_views([Path | Rest], Views) ->
	make_views(Rest, [make_view(Path) | Views]).

make_view(Path) ->
	%% TODO: Add module ID
	ModuleId = generate_module_id(),
	add_module_id(ModuleId).
	
	%% TODO: Add view compiled from script

%% ----------------------------------------------------------------------------
% @spec make_controllers(Paths) -> {ModuleId, Erlang module)
% @doc Compile controllers from provided paths
%% ----------------------------------------------------------------------------		
make_controllers(Paths) ->
	make_controllers(Paths, []).

make_controllers([], Controllers) ->
	Controllers;
make_controllers([Path | Rest], Controllers) ->
	make_controllers(Rest, [make_controller(Path) | Controllers]).

make_controller(Path) ->
	%% TODO: Add module ID
	ModuleId = generate_module_id(),
	add_module_id(ModuleId).
	
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
	
add_model(Model, function, Arguments) ->
	ok.

add_view(View, Arguments) ->
	ok.
add_controller(Controller, function, Arguments) ->
	ok.
