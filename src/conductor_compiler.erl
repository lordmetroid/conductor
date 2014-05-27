-module(conductor_compiler).

-export([
	make/1
]).

%% ----------------------------------------------------------------------------
% @spec make(Paths) -> [{Filename, calendar:datetime(), ModuleId}, ...]
% @doc Compile a modules from specified file paths
%% ----------------------------------------------------------------------------
make(Paths) ->
	make(Paths, []).

make([], Modules) ->
	%% Return compiled modules
	Modules;
make([Path | Rest], Modules) ->
	%% Compile a module from source file
	make(Rest, [make_module(Path) | Modules]).

make_module(Path) ->
	%% Get module location, filename and timestamp
	ModuleLocation = filename:dirname(Path),
	ModuleFilename = filename:basename(Path),
	ModuleModified = filelib:last_modified(Path),

	%% Generate a dynamic module id
	ModuleId = generate_module_id(),
	add_module_id(ModuleId).

	%% Identify module type based on location
	case ModuleLocation of
		conductor_settings:get(program_path) ->
			%% Compile a program module
		conductor_settings:get(model_path) ->
			%% Compile a model module
		conductor_settings:get(view_path) ->
			%% Compile a view module
		conductor_settings:get(controller_path) ->
			%% Compile a controller module
	end.
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
