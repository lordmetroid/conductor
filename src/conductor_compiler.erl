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
	ModuleRoot = filename:dirname(Path),
	ModuleFile = filename:basename(Path),
	ModuleDate = filelib:last_modified(Path),

	%% Generate a dynamic module id
	ModuleId = uuid:uuid_to_string(uuid:get_v4()),

	%% Identify the module type based on location
	case ModuleLocation of
		conductor_settings:get(program_root) ->
			%% Compile a program
			[
				add_module_attribute(ModuleId),
				add_run_function(),
			]
			
		conductor_settings:get(model_root) ->
			%% Compile a model
			add_module_attribute(ModuleId),
			%% TODO: File content goes here
			
		conductor_settings:get(view_root) ->
			%% Compile a view
			add_module_attribute(ModuleId),
			%% TODO: File content goes here
			
		conductor_settings:get(controller_root) ->
			%% Compile a controller
			add_module_attribute(ModuleId),
			%% TODO: File content goes here
			add_run_function(),
			add_data_function(),
			add_render_function(),
			
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

%% TODO: What if Model, View or Controller and function does not exist???

%% ----------------------------------------------------------------------------
% @spec add_data_function() -> erl_syntax()
% @doc Add a data calling function to the compilation
%% ----------------------------------------------------------------------------
add_data_function() ->
	%% data(ModelFile, Function, Arguments) ->
	erl_syntax:function(erl_syntax:atom(data), [
		erl_syntax:clause([
			erl_syntax:variable("ModelFile"),
			erl_syntax:variable("Function"),
			erl_syntax:variable("Arguments")
		], none, [
			%% conductor_router:execute_model(ModelFile, Function, Arguments)
			erl_syntax:application(
				erl_syntax:module_qualifier(
					erl_syntax:atom(conductor_router),
					erl_syntax:atom(execute_model),
				), [
				erl_syntax:variable("ModelFile"),
				erl_syntax:variable("Function"),
				erl_syntax:variable("Arguments")
			])
		])
	]).

%% ----------------------------------------------------------------------------
% @spec add_render_function() -> erl_syntax()
% @doc Add a view render function to the compilation
%% ----------------------------------------------------------------------------
add_render_function() ->
	%% render(ViewFile, Arguments, Response) ->
	erl_syntax:function(erl_syntax:atom(render), [
		erl_syntax:clause([
			erl_syntax:variable("ViewFile"),
			erl_syntax:variable("Arguments"),
			erl_syntax:variable("Response")
		], none, [
			%% conductor_router:execute_view(ViewFile, Arguments, Response)
			erl_syntax:application(
				erl_syntax:module_qualifier(
					erl_syntax:atom(conductor_router),
					erl_syntax:atom(execute_view),
				), [
				erl_syntax:variable("ViewFile"),
				erl_syntax:variable("Arguments"),
				erl_syntax:variable("Response")
			])
		])
	]).

%% ----------------------------------------------------------------------------
% @spec add_run_function() -> erl_syntax()
% @doc Add a run calling function to the compilation
%% ----------------------------------------------------------------------------
add_run_function() ->
	%% run(ControllerFile, Function, Arguments, Response) ->
	erl_syntax:function(erl_syntax:atom(run), [
		erl_syntax:clause([
			erl_syntax:variable("ControllerFile"),
			erl_syntax:variable("Function"),
			erl_syntax:variable("Arguments"),
			erl_syntax:variable("Response")
		], none, [
			%% conductor_router:execute_controller(
			%% 		ControllerFile, Function, Arguments, Response
			%% )
			erl_syntax:application(
				erl_syntax:module_qualifier(
					erl_syntax:atom(conductor_router),
					erl_syntax:atom(execute_controller),
				), [
				erl_syntax:variable("ControllerFile"),
				erl_syntax:variable("Function"),
				erl_syntax:variable("Arguments"),
				erl_syntax:variable("Response")
			])
		])
	]).
