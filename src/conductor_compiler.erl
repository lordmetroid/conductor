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

	%% Identify module type based on location
	case ModuleLocation of
		conductor_settings:get(program_path) ->
			%% Compile a program
			add_module_attribute(ModuleId),
			add_program_export_attribute(),
			add_controller_call_function(),
			
		conductor_settings:get(model_path) ->
			%% Compile a model
			add_module_attribute(ModuleId),
			
		conductor_settings:get(view_path) ->
			%% Compile a view
			add_module_attribute(ModuleId),
			
		conductor_settings:get(controller_path) ->
			%% Compile a controller
			add_module_attribute(ModuleId),
			add_model_call_function(),
			add_view_call_function(),
			
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
% @spec add_module_attribute() -> erl_syntax()
% @doc Add an Erlang module attribute
%% ----------------------------------------------------------------------------
add_module_attribute(ModuleId) ->
	erl_syntax:attribute(erl_syntax:atom(module), [
		erl_syntax:atom(ModuleId)
	]).

%% PROGRAM --------------------------------------------------------------------
	
%% ----------------------------------------------------------------------------
% @spec add_program_export_attribute() -> erl:syntax()
% @doc Add an Erlang module export attribute for a program component
%% ----------------------------------------------------------------------------
add_program_export_attribute() ->
	erl_syntax:attribute(erl_syntax:atom(export), [
		erl_syntax:list([
			erl_syntax:arity_qualifier(
				erl_syntax:atom("execute"),
				erl_syntax:integer(2)
			)
		])
	]).


add_program_execute_function(Body) ->
	erl_syntax:function(erl_syntax:atom(execute), [
		erl_syntax:clause([
			erl_syntax:variable("Parameters"), 
			erl_syntax:variable("Response")
			], [], Body
		)
	]).
	
add_model_call_function() ->
	erl_syntax:function(erl_syntax:atom(model), [
		erl_syntax:clause([
			erl_syntax:clause("ModelName"),
			erl_syntax:clause("FunctionName"),
			erl_syntax:clause("Parameters")
		])
	]).
	
add_view_call_function() ->
	erl_syntax:function(erl_syntax:atom(view), [
		erl_syntax:clause([
			erl_syntax:variable("ViewName"),
			erl_syntax:variable("Parameters"),
			erl_syntax:variable("Response")
		
		], [], %% TODO: Call View
		)
	]).
	
add_controller_call_function() ->
	erl_syntax:function(erl_syntax:atom(controller), [
		erl_syntax:clause([
			erl_syntax:variable("ControllerName"),
			erl_syntax:variable("Parameters"),
			erl_syntax:variable("Response")
		], [], %% TODO: Call controller
		)
	]).
