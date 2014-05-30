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
	ModuleId = uuid:uuid_to_string(uuid:get_v4()),

	%% Identify the module type based on location
	case ModuleLocation of
		conductor_settings:get(program_path) ->
			%% Compile a program
			[
				add_module_attribute(ModuleId),
				add_program_export_attribute(),
				add_controller_call_function()
			]
			
			
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
% @spec add_module_attribute() -> erl_syntax()
% @doc Add an Erlang module attribute to the compilation
%% ----------------------------------------------------------------------------
add_module_attribute(ModuleId) ->
	%% -module(ModuleId)
	erl_syntax:attribute(erl_syntax:atom(module), [
		erl_syntax:atom(ModuleId)
	]).
	
%% ----------------------------------------------------------------------------
% @spec add_program_export_attribute() -> erl_syntax()
% @doc Add an Erlang module export attribute to the compilation
%% ----------------------------------------------------------------------------
add_program_export_attribute() ->
	%% -export([execute/2])
	erl_syntax:attribute(erl_syntax:atom(export), [
		erl_syntax:list([
			erl_syntax:arity_qualifier(
				erl_syntax:atom("execute"),
				erl_syntax:integer(2)
			)
		])
	]).

%% ----------------------------------------------------------------------------
% @spec add_program_execute_function() -> erl_syntax()
% @doc Add a program execute function to the compilation
%% ----------------------------------------------------------------------------
add_program_execute_function(Body) ->
	%% execute(Parameters, Response) ->
	erl_syntax:function(erl_syntax:atom(execute), [
		erl_syntax:clause([
			erl_syntax:variable("Parameters"), 
			erl_syntax:variable("Response")
		], [], 
		%% Dynamic body based on content of file
		Body
		)
	]).

add_model_execute_function() ->
	

%% ----------------------------------------------------------------------------
% @spec add_model_function() -> erl_syntax()
% @doc Add a model calling function to the compilation
%% ----------------------------------------------------------------------------
add_model_function() ->
	%% model(ModelName, Arguments) ->
	erl_syntax:function(erl_syntax:atom(model), [
		erl_syntax:clause([
			erl_syntax:variable("ModelName"),
			erl_syntax:variable("Arguments")
		], none, [
			%% Model = conductor_cache:get_model(ModelName)
			erl_syntax:match_expr(
				erl_syntax:variable("Model"),
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_cache),
						erl_syntax:atom(get_model)
					), [
						erl_syntax:variable("ModelName")
					]
				)
			),
			%% Model:execute(Arguments)
			erl_syntax:application(erl_syntax:module_qualifier(
				erl_syntax:variable("Model"),
				erl_syntax:atom(execute)), [
					erl_syntax:variable("Arguments")
				]
			)
		])
	]).

%% ----------------------------------------------------------------------------
% @spec add_render_function() -> erl_syntax()
% @doc Add a view calling function to the compilation
%% ----------------------------------------------------------------------------
add_render_function() ->
	%% render(ViewName, Arguments, Response) ->
	erl_syntax:function(erl_syntax:atom(render), [
		erl_syntax:clause([
			erl_syntax:variable("ViewName"),
			erl_syntax:variable("Arguments"),
			erl_syntax:variable("Response")
		], none, [
		%% View = conductor_cache:get_view(ViewName)
			erl_syntax:match_expr(
				erl_syntax:variable("View"),
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_cache),
						erl_syntax:atom(get_view)
					), [
						erl_syntax:variable("ViewName")
					]
				)
			),
			%% View:render(Arguments, Response)
			erl_syntax:application(
				erl_syntax:module_qualifier(
					erl_syntax:variable("View"),
					erl_syntax:atom(render)
				), [
					erl_syntax:variable("Arguments"),
					erl_syntax:variable("Response")
				]
			)
		])
	]).

%% ----------------------------------------------------------------------------
% @spec add_controller_function() -> erl_syntax()
% @doc Add a program calling function to the compilation
%% ----------------------------------------------------------------------------
add_controller_function() ->
	%% controller(ControllerName, Arguments, Response) ->
	erl_syntax:function(erl_syntax:atom(controller), [
		erl_syntax:clause([
			erl_syntax:variable("ControllerName"),
			erl_syntax:variable("Arguments"),
			erl_syntax:variable("Response")
		], none, [
			%% Controller = conductor_cache:get_controller(ControllerName)
			erl_syntax:match_expr(
				erl_syntax:variable("Controller"),
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_cache),
						erl_syntax:atom(get_controller)
					), [
						erl_syntax:variable("ControllerName")
					]
				)
			),
			%% Controller:execute(Arguments, Response)
			erl_syntax:application(
				erl_syntax:module_qualifier(
					erl_syntax:variable("Controller"),
					erl_syntax:atom(execute)
				), [
					erl_syntax:variable("Arguments"),
					erl_syntax:variable("Response")
				]
			)
		])
	]).
