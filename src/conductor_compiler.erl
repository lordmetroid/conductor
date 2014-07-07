-module(conductor_compiler).

-export([
	make/1
]).

%% ----------------------------------------------------------------------------
% @spec make_module(ModulePath) -> {ok, {ModuleId, date()}, Binary} | error
% @doc Compile a module from specified file path
%% ----------------------------------------------------------------------------
make(ModulePath) ->
	%% Get module location			
	ProgramRoot = conductor_settings:get(program_root),
	ModelRoot = conductor_settings:get(model_root),
	ViewRoot = conductor_settings:get(view_root),
	ControllerRoot = conductor_settings:get(controller_root),
	
	ModuleRoot = filename:dirname(ModulePath),
	
	%% Assemble module appropiately based on type
	case ModuleRoot of
		ProgramRoot ->
			%% Compile a program
			ModuleForms = lists:flatten([
				add_module_attribute(),
				add_webmachine_lib_attribute(),
				add_file(ModulePath),
				add_run_function()
			]),
			make_module(ModulePath, ModuleForms);
		ModelRoot ->
			%% Compile a model
			ModuleForms = lists:flatten([
				add_module_attribute(),
				add_webmachine_lib_attribute(),
				add_file(ModulePath)
			]),
			make_module(ModulePath, ModuleForms);
		ViewRoot ->
			%% Compile a view
			ModuleForms = lists:flatten([
				add_module_attribute(),
				add_view_export_attribute(),
				add_view(ModulePath)
			]),
			make_module(ModulePath, ModuleForms);
		ControllerRoot ->
			%% Compile a controller
			ModuleForms = lists:flatten([
				add_module_attribute(),
				add_webmachine_lib_attribute(),
				add_file(ModulePath),
				add_run_function(),
				add_data_function(),
				add_render_function()
			]),
			make_module(ModulePath, ModuleForms)
	end.

%% ----------------------------------------------------------------------------
% @spec make() -> {ok, {ModuleId,date()}, Binary} | error
% @doc Add an Erlang module attribute to the compilation
%% ----------------------------------------------------------------------------
make_module(ModulePath, ModuleForms) ->
	case compile:forms(ModuleForms) of
		error ->
			%% Report erlang syntax compilation errors
			conductor_log:add(ModulePath, "Could not compile erlang syntax"),
			error;
		{error, Errors, Warnings} ->
			%% Report erlang syntax compilation errors
			conductor_log:add(ModulePath, [Errors | Warnings]),
			error;
		{ok, Module, ModuleBinary, Warnings} ->
			%% Report erlang syntax compilation errors
			conductor_log:add(ModulePath, Warnings),
			{ok, Module, ModuleBinary};
		{ok, Module, ModuleBinary} ->
			%% Return compiled module 
			{ok, Module, ModuleBinary}
	end.

%% ----------------------------------------------------------------------------
% General module code
%% ----------------------------------------------------------------------------
	
%% ----------------------------------------------------------------------------
% @spec add_module_attribute() -> syntaxTree()
% @doc Add an Erlang module attribute to the compilation
%% ----------------------------------------------------------------------------
add_module_attribute() ->
	erl_syntax:revert(
		%% -module(uuid())
		erl_syntax:attribute(erl_syntax:atom(module), [
			erl_syntax:atom(uuid:uuid_to_string(uuid:get_v4()))
		])
	).

%% ----------------------------------------------------------------------------
% View module code
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec add_view_export_attribute() -> syntaxTree()
% @doc Add the export attribute for rending views
%% ----------------------------------------------------------------------------
add_view_export_attribute() ->
	erl_syntax:revert(
		%% -export([get/0]).
		erl_syntax:attribute(erl_syntax:atom(export), [
			erl_syntax:list([
				erl_syntax:arity_qualifier(
					erl_syntax:atom(get),
					erl_syntax:integer(0)
				)
			])
		])
	).

%% ----------------------------------------------------------------------------
% @spec add_view(ModulePath) -> erl_syntax()
% @doc Add a view file to the compilation
%% ----------------------------------------------------------------------------
add_view(ModulePath) ->
	case file:read_file(ModulePath) of
		{error, Errors} ->
			%% Unable to read file
			conductor_log:add(ModulePath, "Could not read file"),

			%% Return dummy function
			add_view_get_function(error, []);
		{ok, Binary} ->
			%% Convert binary to scannable string
			
			%% TODO: Detect encoding of file
			String = unicode:characters_to_list(Binary, utf8),
			
			%% Get compiler and the template
			case lists:split(string:str(String, "\n")-1, String) of
				{"#!" ++ CompilerName, Template} ->
					%% Compile template
					Compiler = list_to_atom(string:strip(CompilerName)),
					case Compiler:make(Template) of
						{error, Errors} ->
							%% Template could not be compiled
							conductor_log:add(ModulePath, Errors),
						
							%% Return empty function
							add_view_get_function(Compiler, []);
						{ok, ViewAST} ->
							%% Return template as function
							add_view_get_function(Compiler, ViewAST);
						_ ->
							%% Make function does not comply to API
							conductor_log:add(atom_to_list(Compiler),
								"Invalid value from function make"),

							%% Return dummy function
							add_view_get_function(Compiler, [])
					end;
				_ ->
					%% No compiler defined in template file
					conductor_log:add(ModulePath,
						"No compiler specified in template"),

					%% Return dummy function
					add_get_function(error, [])
			end
	end.

%% ----------------------------------------------------------------------------
% @spec add_view_get_function(Compiler, Template) -> syntaxTree()
% @doc Add the Webmachine API library
%% ----------------------------------------------------------------------------
add_get_function(Compiler, Template) ->
	erl_syntax:revert(
		%% get() ->
		erl_syntax:function(erl_syntax:atom(get), [
			erl_syntax:clause([
			], none, [
				%% {Compiler, Template}
				erl_syntax:tuple([
					erl_syntax:atom(Compiler),
					erl_syntax:list(Template)
				])
			])
		])
	).

%% ----------------------------------------------------------------------------
% Program, Model and Controller module code
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec add_webmachine_lib_attribute() -> syntaxTree()
% @doc Add the Webmachine API library
%% ----------------------------------------------------------------------------
add_webmachine_lib_attribute() ->
	erl_syntax:revert(
		%% -include_lib("webmachine/include/webmachine.hrl").
		erl_syntax:attribute(erl_syntax:atom(include_lib), [
			erl_syntax:string("webmachine/include/webmachine.hrl")
		])
	).

%% ----------------------------------------------------------------------------
% @spec add_file() -> syntaxTree()
% @doc Add an Erlang File to the compilation
%% ----------------------------------------------------------------------------
add_file(ModulePath) ->
	case file:read_file(ModulePath) of
		{error, Errors} ->
			conductor_log:add(ModulePath, "Could not read file"),

			%% Return nothing
			[];
		{ok, Binary} ->
			File = unicode:characters_to_list(Binary, utf8),
			dynamic_compile:forms_from_string(File)
	end.

%% ----------------------------------------------------------------------------
% @spec add_data_function() -> syntaxTree()
% @doc Add a data calling function to the compilation
%% ----------------------------------------------------------------------------
add_data_function() ->
	[
		erl_syntax:revert(
			%% data(ModelFile, Function) ->
			erl_syntax:function(erl_syntax:atom(data), [
				erl_syntax:clause([
					erl_syntax:variable("ModelFile"),
					erl_syntax:variable("Function")
				], none, [
					%% conductor_router:execute_model(ModelFile, Function, [])
					erl_syntax:application(
						erl_syntax:module_qualifier(
							erl_syntax:atom(conductor_router),
							erl_syntax:atom(execute_model)
						), [
						erl_syntax:variable("ModelFile"),
						erl_syntax:variable("Function"),
						erl_syntax:list([])
					])
				])
			])
		),
		erl_syntax:revert(
			%% data(ModelFile, Function, Arguments) ->
			erl_syntax:function(erl_syntax:atom(data), [
				erl_syntax:clause([
					erl_syntax:variable("ModelFile"),
					erl_syntax:variable("Function"),
					erl_syntax:variable("Arguments")
				], none, [
					%% conductor_router:execute_model(
					%%		ModelFile, Function, Arguments)
					erl_syntax:application(
						erl_syntax:module_qualifier(
							erl_syntax:atom(conductor_router),
							erl_syntax:atom(execute_model)
						), [
						erl_syntax:variable("ModelFile"),
						erl_syntax:variable("Function"),
						erl_syntax:variable("Arguments")
					])
				])
			])
		)
	].

%% ----------------------------------------------------------------------------
% @spec add_render_function() -> syntaxTree()
% @doc Add a view render function to the compilation
%% ----------------------------------------------------------------------------
add_render_function() ->
	[
		erl_syntax:revert(
			%% render(ViewFile) ->
			erl_syntax:function(erl_syntax:atom(render), [
				erl_syntax:clause([
					erl_syntax:variable("ViewFile")
				], none, [
					%% conductor_router:execute_view(ViewFile, [])
					erl_syntax:application(
						erl_syntax:module_qualifier(
							erl_syntax:atom(conductor_router),
							erl_syntax:atom(execute_view)
						), [
						erl_syntax:variable("ViewFile"),
						erl_syntax:list([])
					])
				])
			])
		),
		erl_syntax:revert(
			%% render(ViewFile, Arguments) ->
			erl_syntax:function(erl_syntax:atom(render), [
				erl_syntax:clause([
					erl_syntax:variable("ViewFile"),
					erl_syntax:variable("Arguments")
				], none, [
					%% conductor_router:execute_view(ViewFile, Arguments)
					erl_syntax:application(
						erl_syntax:module_qualifier(
							erl_syntax:atom(conductor_router),
							erl_syntax:atom(execute_view)
						), [
						erl_syntax:variable("ViewFile"),
						erl_syntax:variable("Arguments")
					])
				])
			])
		)
	].

%% ----------------------------------------------------------------------------
% @spec add_run_function() -> syntaxTree()
% @doc Add a run calling function to the compilation
%% ----------------------------------------------------------------------------
add_run_function() ->
	[
		erl_syntax:revert(
			%% run(ControllerFile, Function) ->
			erl_syntax:function(erl_syntax:atom(run), [
				erl_syntax:clause([
					erl_syntax:variable("ControllerFile"),
					erl_syntax:variable("Function")
				], none, [
					%% conductor_router:execute_controller(
					%% 		ControllerFile, Function, [])
					erl_syntax:application(
						erl_syntax:module_qualifier(
							erl_syntax:atom(conductor_router),
							erl_syntax:atom(execute_controller)
						), [
						erl_syntax:variable("ControllerFile"),
						erl_syntax:variable("Function"),
						erl_syntax:list([])
					])
				])
			])
		),
		erl_syntax:revert(
			%% run(ControllerFile, Function, Arguments) ->
			erl_syntax:function(erl_syntax:atom(run), [
				erl_syntax:clause([
					erl_syntax:variable("ControllerFile"),
					erl_syntax:variable("Function"),
					erl_syntax:variable("Arguments")
				], none, [
					%% conductor_router:execute_controller(
					%% 		ControllerFile, Function, Arguments)
					erl_syntax:application(
						erl_syntax:module_qualifier(
							erl_syntax:atom(conductor_router),
							erl_syntax:atom(execute_controller)
						), [
						erl_syntax:variable("ControllerFile"),
						erl_syntax:variable("Function"),
						erl_syntax:variable("Arguments")
					])
				])
			])
		)
	].


