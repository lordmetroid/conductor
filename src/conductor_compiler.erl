-module(conductor_compiler).

-export([
	make_modules/1,
	make_module/1
]).

%% ----------------------------------------------------------------------------
% @spec make(Paths) -> [{Filename, calendar:datetime(), ModuleId}, ...]
% @doc Compile a modules from specified file paths
%% ----------------------------------------------------------------------------
make_modules(ModulePaths) ->
	%% Compile a collection of modules
	make(ModulePaths, []).

make_modules([], Modules) ->
	%% Return compiled modules
	Modules;

make_modules([ModulePath | Rest], Modules) ->
	%% Compile a module from source file
	case make_module(ModulePath) of
		error ->
			%% Do not include uncompiled modules
			make_modules(Rest, Modules);
		{Module, ModuleDate} ->
			%% Add compiled module to the collection
			make_modules(Rest, [{Module, ModuleDate} | Modules])
	end.

make_module(ModulePath) ->
	%% Get module location and timestamp
	ModuleRoot = filename:dirname(ModulePath),
	ModuleDate = filelib:last_modified(ModulePath),
	
	%% Generate a dynamic module id
	ModuleId = uuid:uuid_to_string(uuid:get_v4()),

	%% Identify the module type based on location
	ProgramRoot = conductor_settings:get(program_root),
	ModelRoot = conductor_settings:get(model_root),
	ViewRoot = conductor_settings:get(view_root),
	ControllerRoot = conductor_settings:get(controller_root),

	case ModuleRoot of
		ProgramRoot ->
			%% Compile a program
			ProgramForm = [
				add_module_attribute(ModuleId),
				add_webmachine_lib_attribute(),
				add_file(ModulePath),
				add_run_function()
			],
			compile_module(ProgramForm, ModuleDate);
		ModelRoot ->
			%% Compile a model
			ModelForm = [
				add_module_attribute(ModuleId),
				add_webmachine_lib_attribute(),
				add_file(ModulePath)
			],
			compile_module(ModelForm, ModuleDate);
		ViewRoot ->
			%% Compile a view
			ViewForm = [
				add_module_attribute(ModuleId),
				add_view(ModulePath)
			],
			compile_module(ViewForm, ModuleDate);
		ControllerRoot ->
			%% Compile a controller
			ControllerForm = [
				add_module_attribute(ModuleId),
				add_webmachine_lib_attribute(),
				add_file(ModulePath),
				add_run_function(),
				add_data_function(),
				add_render_function()
			],
			compile_module(ControllerForm, ModuleDate)
	end.

compile_module(Forms, ModuleDate) ->
	case compile:forms(Forms) of
		error ->
			%% TODO: Write compilation failure to log
			error;
		{error, Errors, Warnings} ->
			%% TODO: Write errors to log
			%% TODO: Write warnings to log
			error;
		{ok, Module, ModuleBinary} ->
			load_module(Module, ModuleBinary, ModuleDate);
		{ok, Module, ModuleBinary, Warnings} ->
			%% TODO: Write warnings to log
			load_module(Module, ModuleBinary, ModuleDate)
	end.
	
load_module(Module, ModuleBinary, ModuleDate) ->
	case code:load_binary(Module, [], ModuleBinary) of
		{error, Error} ->
			%% TODO: Write errors to log
			error;
		{module, Module} ->
			%% Return module
			{Module, ModuleDate}
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
			%% TODO: Write to error to log

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
% @spec add_view() -> syntaxTree()
% @doc Add an Erlang File to the compilation
%% ----------------------------------------------------------------------------
add_view(ModulePath) ->
	%% Compile view template
	ViewCompiler = conductor_settings:get(view_compiler),
	ViewCompiler:make(ModulePath).

%% ----------------------------------------------------------------------------
% @spec add_data_function() -> syntaxTree()
% @doc Add a data calling function to the compilation
%% ----------------------------------------------------------------------------
add_data_function() ->
	%% data(ModelFile, Function, Arguments, Parameters) ->
	erl_syntax:revert(erl_syntax:function(erl_syntax:atom(data), [
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
	])).

%% ----------------------------------------------------------------------------
% @spec add_render_function() -> syntaxTree()
% @doc Add a view render function to the compilation
%% ----------------------------------------------------------------------------
add_render_function() ->
	%% render(ViewFile, Arguments) ->
	erl_syntax:revert(erl_syntax:function(erl_syntax:atom(render), [
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
	])).

%% ----------------------------------------------------------------------------
% @spec add_run_function() -> syntaxTree()
% @doc Add a run calling function to the compilation
%% ----------------------------------------------------------------------------
add_run_function() ->
	%% run(ControllerFile, Function, Arguments, Parameters) ->
	erl_syntax:revert(erl_syntax:function(erl_syntax:atom(run), [
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
	])).

