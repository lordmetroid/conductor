-module(conductor_compiler).

-export([
	make_programs/1,
	make_program/1,
	make_models/1
	make_model/1,
	make_views/1,
	make_view/1,
	make_controllers/1,
	make_controller/1
]).

%% ----------------------------------------------------------------------------
% @spec make_programs
% @doc Compile programs from provided paths
%% ----------------------------------------------------------------------------
make_programs(Paths) ->
	make_programs(Paths, []).
make_programs([], Programs) ->
	Programs;
make_programs([Path | Rest], Programs) ->
	make_programs(Rest, [make_program(Path) | Programs]).
	
make_program(Path)
	%% Compile a program component
	
	%% TODO: Add module ID
	
	%% TODO: Add execute function header

%% ----------------------------------------------------------------------------
% @spec make_models
% @doc Compile models from provided paths
%% ----------------------------------------------------------------------------
make_models(Paths) ->
	make_models(Paths, []).
make_models([], Models) ->
	Models;
make_models([Path | Rest], Models) ->
	make_models(Rest, [make_model(Path) | Models]).

make_model(Path) ->
	%% Compile a model component
	
	%% TODO: Add module ID
	
%% ----------------------------------------------------------------------------
% @spec make_views
% @doc Compile views from provided paths
%% ----------------------------------------------------------------------------
make_views(Paths) ->
	make_views(Paths, []).
make_views([], Views) ->
	Views;
make_views([Path | Rest], Views) ->
	make_views(Rest, [make_view(Path) | Views]).

make_view(Path) ->
	%% Compile a view component
	
	%% TODO: Add module ID
	
	%% TODO: Add view compiled from script

%% ----------------------------------------------------------------------------
% @spec make_controllers
% @doc Compile controllers from provided paths
%% ----------------------------------------------------------------------------		
make_controllers(Paths) ->
	make_controllers(Paths, []).
make_controllers([], Controllers) ->
	Controllers;
make_controllers([Path | Rest], Controllers) ->
	make_controllers(Rest, [make_controller(Path) | Controllers]).

make_controller(Path) ->
	%% Compile a controller component
	
	%% TODO: Add module ID
	
	%% TODO: Add controller standard helper function

%% ----------------------------------------------------------------------------
% Compilation functions
%% ----------------------------------------------------------------------------
	
%% ----------------------------------------------------------------------------
% @spec module_id() -> UUID::string()
% @doc Compile half an erlang file into a random module
%% ----------------------------------------------------------------------------
module_id() ->
	uuid:uuid_to_string(uuid:get_v4()).

	erl_syntax:attribute(erl_syntax:atom(module), [
		erl_syntax:atom(module_id())
	]).
