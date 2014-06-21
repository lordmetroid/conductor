-module(conductor_compiler).

-export([
	make_modules/1,
	make_module/1
]).

%% ----------------------------------------------------------------------------
% @spec make_modules(ModulePaths) -> {[{ModuleId, datetime()}, ...], [Errors]}
% @doc Compile modules from a list of specified file paths
%% ----------------------------------------------------------------------------
make_modules(ModulePaths) ->
	%% Compile a collection of modules
	make_modules(ModulePaths, [], []).

make_modules([], Modules, Errors) ->
	%% Return compiled modules and errors
	{Modules, Errors};
make_modules([ModulePath | Rest], Modules, Errors) ->
	%% Compile a module from source file
	case make_module(ModulePath) of
		{error, NewErrors} ->
			%% Do not include uncompiled module and report error
			make_modules(Rest, Modules, [NewErrors | Errors]);
		{error, NewErrors, Warnings} ->
			%% Do not include uncompiled module and report errors and warnings
			make_modules(Rest, Modules, [Warnings | [NewErrors | Errors]]);
		{ok, NewModule, Warnings} ->
			%% Add compiled module and report warnings
			make_modules(Rest, [NewModule | Modules], [Warnings | Errors]);
		{ok, NewModule} ->
			%% Add compiled module
			make_modules(Rest, [NewModule | Modules], Errors)
	end.

%% ----------------------------------------------------------------------------
% @spec make_module(ModulePath) -> {ModuleId, datetime()}
% @doc Compile a module from specified file path
%% ----------------------------------------------------------------------------
make_module(ModulePath) ->
	case filelib:last_modified(ModulePath) of
		0 ->
			%% Module file could not be found
			{error, ModulePath ++ " not found"};
		Date ->
			%% Module file exist
			
			%% Generate a dynamic module id
			ModuleId = uuid:uuid_to_string(uuid:get_v4()),
			
			%% Get module location			
			ProgramRoot = conductor_settings:get(program_root),
			ModelRoot = conductor_settings:get(model_root),
			ViewRoot = conductor_settings:get(view_root),
			ControllerRoot = conductor_settings:get(controller_root),
			
			ModuleRoot = filename:dirname(ModulePath),
			
			%% Identify the module type based on location
			case ModuleRoot of
				ProgramRoot ->
					%% Compile a program
					ModuleForms = [
						add_module_attribute(ModuleId),
						add_webmachine_lib_attribute(),
						add_file(ModulePath),
						add_run_function()
					],
					compile_module(ModulePath, ModuleForms, Date);
				ModelRoot ->
					%% Compile a model
					ModuleForms = [
						add_module_attribute(ModuleId),
						add_webmachine_lib_attribute(),
						add_file(ModulePath)
					],
					compile_module(ModulePath, ModuleForms, Date);
				ViewRoot ->
					%% Compile a view
					ModuleForms = [
						add_module_attribute(ModuleId),
						add_view_export_attribute(),
						add_view(ModulePath)
					],
					compile_module(ModulePath, ModuleForms, Date);
				ControllerRoot ->
					%% Compile a controller
					ModuleForms = [
						add_module_attribute(ModuleId),
						add_webmachine_lib_attribute(),
						add_file(ModulePath),
						add_run_function(),
						add_get_function(),
						add_render_function()
					],
					compile_module(ModulePath, ModuleForms, Date)
			end
	end.

compile_module(ModulePath, ModuleForms, Date) ->
	case compile:forms(ModuleForms) of
		error ->
			%% Report erlang syntax compilation error 
			{error, "Could not compile erlang syntax for " ++ ModulePath};
		{error, Errors, Warnings} ->
			%% Report erlang syntax compilation error and warnings
			{error,  Errors, Warnings};
		{ok, Module, ModuleBinary, Warnings} ->
			%% Load module in spite of warnings and report warnings
			{ok, {Module,Date}, ModuleBinary, Warnings};
		{ok, Module, ModuleBinary} ->
			%% Load module
			{ok, {Module,Date}, ModuleBinary}
	end.

%% ----------------------------------------------------------------------------
% @spec add_module_attribute() -> syntaxTree()
% @doc Add an Erlang module attribute to the compilation
%% ----------------------------------------------------------------------------
add_module_attribute(ModuleId) ->
	erl_syntax:revert(
		%% -module(ModuleId)
		erl_syntax:attribute(erl_syntax:atom(module), [
			erl_syntax:atom(ModuleId)

		])
	).

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
		{error, Reason} ->
			%% TODO: Write error to log

			%% Nothing to return
			[];
		{ok, Binary} ->
			FileString = unicode:characters_to_list(Binary, utf8),
			case erl_scan:string(FileString) of
				{error, ErrorInfo, ErrorLocation} ->
					%% TODO: Write errors to log

					%% Nothing to return
					[];
				{ok, FileAST, _EndLocation} ->
					erl_syntax:revert(FileAST)
			end
	end.

%% ----------------------------------------------------------------------------
% @spec add_view_export_attribute() -> syntaxTree()
% @doc Add the export attribute for rending views
%% ----------------------------------------------------------------------------
add_view_export_attribute() ->
	erl_syntax:revert(
		%% -export([get/1]).
		erl_syntax:attribute(erl_syntax:atom(export), [
			erl_syntax:list([
				erl_syntax:arity_qualifier(
					erl_syntax:atom(get),
					erl_syntax:integer(1)
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
			{error, Errors};
		{ok, Binary} ->
			%% Convert binary to scannable string
			%% TODO: Detect encoding of file
			String = unicode:characters_to_list(Binary, utf8),
			
			%% Get compiler and the template
			case lists:split(string:str(String, "\n")-1, String) of
				{"#!" ++ CompilerString, Template} ->
					%% Compile view
					Compiler = list_to_atom(string:strip(CompilerString)),

					case Compiler:make(Template) of
						{error, Reason} ->
							%% View template could not be compiled
							%% TODO: Report error
						
							%% Nothing to return
							erl_syntax:revert(
								%% get() ->
								erl_syntax:function(erl_syntax:atom(get), [
									erl_syntax:clause([
									], none, [
										%% {Compiler, []}
										erl_syntax:tuple([
											erl_syntax:atom(Compiler),
											erl_syntax:lists([])
										])
									])
								])
							);
						{ok, ViewAST} ->
							%% Store view template as function
							erl_syntax:revert(
								%% get() ->
								erl_syntax:function(erl_syntax:atom(get), [
									erl_syntax:clause([
									], none, [
										%% {Compiler, Template}
										erl_syntax:tuple([
											erl_syntax:atom(Compiler),
											ViewAST
										])
									])
								])
							)
					end;
				_ ->
					%% No compiler defined

					%% TODO: Report error

			end
	end.

%% ----------------------------------------------------------------------------
% @spec add_get_function() -> syntaxTree()
% @doc Add a data calling function to the compilation
%% ----------------------------------------------------------------------------
add_get_function() ->
	erl_syntax:revert(
		%% get(ModelFile, Function, Arguments, Parameters) ->
		erl_syntax:function(erl_syntax:atom(get), [
			erl_syntax:clause([
				erl_syntax:variable("ModelFile"),
				erl_syntax:variable("Function"),
				erl_syntax:variable("Arguments"),
				erl_syntax:variable("Parameters")
			], none, [
				%% conductor_router:execute_model(
				%%		ModelFile, Function, Arguments, Parameters
				%% )
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_router),
						erl_syntax:atom(execute_model)
					), [
					erl_syntax:variable("ModelFile"),
					erl_syntax:variable("Function"),
					erl_syntax:variable("Arguments"),
					erl_syntax:variable("Parameters")
				])
			])
		])
	).

%% ----------------------------------------------------------------------------
% @spec add_render_function() -> syntaxTree()
% @doc Add a view render function to the compilation
%% ----------------------------------------------------------------------------
add_render_function() ->
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
	).

%% ----------------------------------------------------------------------------
% @spec add_run_function() -> syntaxTree()
% @doc Add a run calling function to the compilation
%% ----------------------------------------------------------------------------
add_run_function() ->
	erl_syntax:revert(
		%% run(ControllerFile, Function, Arguments, Parameters) ->
		erl_syntax:function(erl_syntax:atom(run), [
			erl_syntax:clause([
				erl_syntax:variable("ControllerFile"),
				erl_syntax:variable("Function"),
				erl_syntax:variable("Arguments"),
				erl_syntax:variable("Parameters")
			], none, [
				%% conductor_router:execute_controller(
				%% 		ControllerFile, Function, Arguments, Parameters
				%% )
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_router),
						erl_syntax:atom(execute_controller)
					), [
					erl_syntax:variable("ControllerFile"),
					erl_syntax:variable("Function"),
					erl_syntax:variable("Arguments"),
					erl_syntax:variable("Parameters")
				])
			])
		])
	).

