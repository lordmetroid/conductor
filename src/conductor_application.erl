-module(conductor_application).
-compile({parse_transform, lager_transform}).

-export([
	execute/1,
	execute_model/3,
	execute_view/2,
	execute_controller/3
]).

-include_lib("webmachine/include/webmachine.hrl").

%% ============================================================================
%% Module functions
%% ============================================================================

%% @doc Execute a client request
%% @spec execute(Request::rd()) -> Content::iolist()
execute(Request) ->
	Path = wrq:path(Request),
	Domain = wrq:host_tokens(Request),
	Programs = conductor_settings:get(Domain, programs),

	case lists:keyfind(Path, 1, Programs) of
		false ->
			publish_file(Request, Domain, Path);
		{Path, ProgramFile} ->
			get_program(Request, Domain, ProgramFile)
	end.

publish_file(Request, Domain, Path) ->
	FileRoot = conductor_settings:get(Domain, file_root),
	FilePath = filename:join([FileRoot, Path]),

	case filelib:is_regular(FilePath) of
		false ->
			false; %% 404 Not Found
		true ->
			conductor_response:create_file(Request),
			conductor_response:add_file_content(FilePath)
	end.

get_program(Request, Domain, ProgramFile) ->
	ProgramRoot = conductor_settings:get(Domain, program_root),
	ProgramPath = filename:join([ProgramRoot, ProgramFile]),

	case conductor_cache:get_module(ProgramPath) of
		false ->
			log_module_not_found_error(ProgramPath);
		Program ->
			execute_program_module(Request, Program)
	end.

execute_program_module(Request, Program) ->
	case erlang:function_exported(Program, execute, 1) of
		false ->
			log_function_not_exported_error(execute, 1);
		true ->
			conductor_response:create_program(Request),
			Program:execute(Request)
	end.

%% ============================================================================
% @doc Execute a model file during the execution of a program
% @spec
execute_model(ModelFile, Function, Arguments) ->
	case conductor_response:get_request() of
		false ->
			false;
		Request ->
			get_model_module(Request, ModelFile, Function, Arguments)
	end.

get_model_module(Request, ModelFile, Function, Arguments) ->
	Domain = wrq:domain_tokens(Request),

	ModelRoot = conductor_settings:get(Domain, model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),
	
	case conductor_cache:get_module(ModelPath) of
		false ->
			log_module_not_found_error(ModelPath);
		Model ->
			execute_model_module(Request, Model, Function, Arguments)
	end.

execute_model_module(Request, Model, Function, Arguments) ->
	case erlang:function_exported(Model, Function, 2) of
		false ->
			log_function_not_exported_error(Function, 2);
		true ->
			Model:Function(Request, Arguments)
	end.

%% ============================================================================
% @doc Render a view file during the execution of a program
% @spec
execute_view(ViewFile, Arguments) ->
	case conductor_response:get_request() of
		false ->
			false;
		Request ->
			get_view_module(Request, ViewFile, Arguments)
	end.

get_view_module(Request, ViewFile, Arguments) ->
	Domain = wrq:domain_tokens(Request),

	ViewRoot = conductor_settings:get(Domain, view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),
	
	case conductor_cache:get_module(ViewPath) of
		false ->
			log_module_not_found_error(ViewPath);
		View ->
			get_view_template(View, Arguments)
	end.

get_view_template(View, Arguments) ->
	case erlang:function_exported(View, get, 0) of
		false ->
			log_function_not_exported_error(get, 0);
		true ->
			{Compiler, Template} = View:get(),
			validate_template(Compiler, Template, Arguments)
	end.

validate_template(Compiler, Template, Arguments) ->
	case erlang:function_exported(Compiler, render, 2) of
		false ->
			log_compiler_error(Compiler);
		true ->
			render_view_template(Compiler, Template, Arguments)
	end.

render_view_template(Compiler, Template, Arguments) ->
	case Compiler:render(Template, Arguments) of
		{error, Content, Errors} ->
			log_render_content_error(Errors),
			conductor_response:add_content(Content);
		{ok, Content} ->
			conductor_response:add_content(Content);	
		_ ->
			log_render_api_error(Compiler)
	end.

%% ============================================================================
%%  @doc Execute a controller file during the execution of a program
%%
execute_controller(ControllerFile, Function, Arguments)  ->
	case conductor_response:get_request() of
		false ->
			false;
		Request ->
			get_controller_module(Request, ControllerFile, Function, Arguments)
	end.

get_controller_module(Request, ControllerFile, Function, Arguments) ->
	Domain = wrq:domain_tokens(Request),

	ControllerRoot = conductor_settings:get(Domain, controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),
	
	case conductor_cache:get_module(ControllerPath) of
		false ->
			log_module_not_found_error(ControllerPath);
		Controller ->
			execute_controller_module(Request, Controller, Function, Arguments)
	end.

execute_controller_module(Request, Controller, Function, Arguments) ->
	case erlang:function_exported(Controller, Function, 2) of
		false ->
			log_function_not_exported_error(Function, 2);
		true ->
			Controller:Function(Request, Arguments)
	end.

%% ============================================================================
%% Logging functions
%% ============================================================================
log_module_not_found_error(Path) ->
	lager:warning("Could not find ~s", [Path]).

log_function_not_exported_error(Function, Arity) ->
	lager:warning("Could not find ~s/~B", [Function, Arity]).

log_compiler_error(error) ->
	lager:warning("Compiler not specified in template");
log_compiler_error(Compiler) ->
	lager:warning("Could not find the render function in ~s", [Compiler]).

log_render_content_error(Errors) ->
	lager:warning("Could not render template ~p", [Errors]).

log_render_api_error(Compiler) ->
	lager:warning("Compiler ~s returned invalid value", [Compiler]).
