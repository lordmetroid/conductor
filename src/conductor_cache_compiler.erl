-module(conductor_cache_compiler).
-compile({parse_transform, lager_transform}).

-export([
	make/2
]).

%% ============================================================================
%% Module functions
%% ============================================================================

%% @doc Compile a module from file to store in the cache
make(program, ModulePath) ->
	ModuleForms = lists:flatten([
		add_module_attribute(),
		add_controller_export_attribute(),
		add_webmachine_lib_attribute(),
		add_file(ModulePath),
		add_run_function(),
		add_data_function(),
		add_render_function()
	]),
	make_module(ModulePath, ModuleForms);
make(model, ModulePath) ->
	ModuleForms = lists:flatten([
		add_module_attribute(),
		add_webmachine_lib_attribute(),
		add_file(ModulePath)
	]),
	make_module(ModulePath, ModuleForms);
make(view, ModulePath) ->
	ModuleForms = lists:flatten([
		add_module_attribute(),
		add_view_export_attribute(),
		add_view(ModulePath)
	]),
	make_module(ModulePath, ModuleForms);
make(controller, ModulePath) ->
	ModuleForms = lists:flatten([
		add_module_attribute(),
		add_controller_export_attribute(),
		add_webmachine_lib_attribute(),
		add_file(ModulePath),
		add_run_function(),
		add_data_function(),
		add_render_function()
	]),
	make_module(ModulePath, ModuleForms).

%% ============================================================================
%% Helper functions
%% ============================================================================

%% @doc Compile module forms to binary
make_module(ModulePath, ModuleForms) ->
	case compile:forms(ModuleForms) of
		error ->
			log_compile_error(ModulePath, [],[]),
			error;
		{error, Errors, Warnings} ->
			log_compile_error(ModulePath, Errors, Warnings),
			error;
		{ok, Module, ModuleBinary, Warnings} ->
			log_compile_error(ModulePath, [], Warnings),
			{Module, ModuleBinary};
		{ok, Module, ModuleBinary} ->
			{Module, ModuleBinary}
	end.

%% ============================================================================
%% General module code
%% ============================================================================

%% @doc Add the -module() attribute
add_module_attribute() ->
	erl_syntax:revert(
		%% -module(uuid())
		erl_syntax:attribute(erl_syntax:atom(module), [
			erl_syntax:atom(uuid:uuid_to_string(uuid:get_v4()))
		])
	).

%% ============================================================================
% View module code
%% ============================================================================

% @doc Add the -export() attribute for rending views
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

% @doc Compile the view and store it as a module function
add_view(ModulePath) ->
	case file:read_file(ModulePath) of
		{error, Reason} ->
			log_read_file_error(ModulePath, Reason),
			add_get_function(error, []);
		{ok, ModuleBinary} ->
			get_view_compiler(ModulePath, ModuleBinary)
	end.

get_view_compiler(ModulePath, ModuleBinary) ->
	%% TODO: Detect encoding of file
	ModuleContent = unicode:characters_to_list(ModuleBinary, utf8),
	
	%% Get compiler and the template
	case lists:split(string:str(ModuleContent, "\n")-1, ModuleContent) of
		{"#!" ++ CompilerName, Template} ->
			Compiler = list_to_atom(
				string:to_lower(
				string:strip(
				string:strip(CompilerName, both, $\r)
			))),

			compile_template(ModulePath, Compiler, Template);
		_InvalidResult ->
			log_template_api_error(ModulePath),
			add_get_function(error, [])
	end.

compile_template(ModulePath, Compiler, Template) ->

	case Compiler:make(Template) of
		{error, Reason} ->
			log_compiler_make_error(ModulePath, Compiler, Reason),
			add_get_function(Compiler, []);
		{ok, ViewAST} ->
			add_get_function(Compiler, ViewAST);
		_InvalidResult ->
			log_compiler_api_error(Compiler),
			add_get_function(Compiler, [])
	end.

% @doc Add the get view 
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

%% ============================================================================
%% Program, Model and Controller module code
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Add the export attribute for model files
%% ----------------------------------------------------------------------------
add_controller_export_attribute() ->
	erl_syntax:revert(
		%% -export([data/3, render/2, run/3]).
		erl_syntax:attribute(erl_syntax:atom(export), [
			erl_syntax:list([
				erl_syntax:arity_qualifier(
					erl_syntax:atom(data),
					erl_syntax:integer(3)
				),
				erl_syntax:arity_qualifier(
					erl_syntax:atom(render),
					erl_syntax:integer(2)
				),
				erl_syntax:arity_qualifier(
					erl_syntax:atom(run),
					erl_syntax:integer(3)
				)
			])
		])
	).


%% ----------------------------------------------------------------------------
%% @doc Add the Webmachine API library
%% ----------------------------------------------------------------------------
add_webmachine_lib_attribute() ->
	erl_syntax:revert(
		%% -include_lib("webmachine/include/webmachine.hrl").
		erl_syntax:attribute(erl_syntax:atom(include_lib), [
			erl_syntax:string("webmachine/include/webmachine.hrl")
		])
	).

%% ----------------------------------------------------------------------------
%% @doc Add an Erlang File to the compilation
%% ----------------------------------------------------------------------------
add_file(ModulePath) ->
	case file:read_file(ModulePath) of
		{error, Reason} ->
			log_read_file_error(ModulePath, Reason),
			[];
		{ok, Binary} ->
			File = unicode:characters_to_list(Binary, utf8),
			dynamic_compile:forms_from_string(File)
	end.

%% ----------------------------------------------------------------------------
%% @doc Add a data calling function to the compilation
%% ----------------------------------------------------------------------------
add_data_function() ->
	erl_syntax:revert(
		%% data(ModelFile, Function, Arguments) ->
		erl_syntax:function(erl_syntax:atom(data), [
			erl_syntax:clause([
				erl_syntax:variable("ModelFile"),
				erl_syntax:variable("Function"),
				erl_syntax:variable("Arguments")
			], none, [
				%% conductor_application:execute_model(
				%%		ModelFile, Function, Arguments)
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_application),
						erl_syntax:atom(execute_model)
					), [
					erl_syntax:variable("ModelFile"),
					erl_syntax:variable("Function"),
					erl_syntax:variable("Arguments")
				])
			])
		])
	).

%% ----------------------------------------------------------------------------
%% @doc Add a view render function to the compilation
%% ----------------------------------------------------------------------------
add_render_function() ->
	erl_syntax:revert(
		%% render(ViewFile, Arguments) ->
		erl_syntax:function(erl_syntax:atom(render), [
			erl_syntax:clause([
				erl_syntax:variable("ViewFile"),
				erl_syntax:variable("Arguments")
			], none, [
				%% conductor_application:execute_view(ViewFile, Arguments)
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_application),
						erl_syntax:atom(execute_view)
					), [
					erl_syntax:variable("ViewFile"),
					erl_syntax:variable("Arguments")
				])
			])
		])
	).

%% ----------------------------------------------------------------------------
%% @doc Add a run calling function to the compilation
%% ----------------------------------------------------------------------------
add_run_function() ->
	erl_syntax:revert(
		%% run(ControllerFile, Function, Arguments) ->
		erl_syntax:function(erl_syntax:atom(run), [
			erl_syntax:clause([
				erl_syntax:variable("ControllerFile"),
				erl_syntax:variable("Function"),
				erl_syntax:variable("Arguments")
			], none, [
				%% conductor_application:execute_controller(
				%% 		ControllerFile, Function, Arguments)
				erl_syntax:application(
					erl_syntax:module_qualifier(
						erl_syntax:atom(conductor_application),
						erl_syntax:atom(execute_controller)
					), [
					erl_syntax:variable("ControllerFile"),
					erl_syntax:variable("Function"),
					erl_syntax:variable("Arguments")
				])
			])
		])
	).

%% ============================================================================
%% Logging functions
%% ============================================================================

log_read_file_error(ModulePath, Reason) ->
	lager:warning("Could read ~s: ~p", [ModulePath, Reason]).

log_compile_error(ModulePath, [], []) ->
	lager:warning("Could not compile ~s", [ModulePath]);
log_compile_error(ModulePath, [Error | Rest], []) ->
	lager:warning("Compile error: ~p", [Error]),
	log_compile_error(ModulePath, Rest, []);
log_compile_error(ModulePath, Errors, [Warning | Rest]) ->
	lager:warning("Compile warning: ~p", [Warning]),
	log_compile_error(ModulePath, Errors, Rest).

log_template_api_error(ModulePath) ->
	lager:warning("Template ~s do specify #! $COMPILER_NAME", [ModulePath]).

log_compiler_make_error(ModulePath, Compiler, []) ->
	lager:warning("Compiler ~p could not compiler ~s", [Compiler, ModulePath]);
log_compiler_make_error(ModulePath, Compiler, [Reason | Rest]) ->
	lager:warning("Compiler ~p: ~s", [ModulePath, Compiler, Reason]),
	log_compiler_make_error(ModulePath, Compiler, Rest).

log_compiler_api_error(Compiler) ->
	lager:warning("Compiler ~p:make/1 invalid result returned", [Compiler]).

