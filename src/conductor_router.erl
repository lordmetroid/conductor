-module(conductor_router).
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
			publish_file(Domain, Path);
		{Path, ProgramFile} ->
			get_program(Request, Domain, ProgramFile).
	end.

publish_file(Domain, Path) ->
	FileRoot = conductor_settings:get(Domain, file_root),
	FilePath = filename:join([FileRoot, Path]),

	case filelib:is_regular(FilePath) of
		false ->
			%% TODO
		true ->
			conductor_response:create_file(),
			conductor_response:add_file_content(FilePath)
	end.

get_program(Request, Domain ProgramFile) ->
	%% Get the program module from cache
	ProgramRoot = conductor_settings:get(Domain, program_root),
	ProgramPath = filename:join([ProgramRoot, ProgramFile]),

	case conductor_cache:get_module(ProgramPath) of
		false ->
			%% TODO
		Program ->
			execute_program_module(Request, Program)
	end.

execute_program_module(Request, Program)
	case erlang:function_exported(Program, execute, 1) of
		false ->
			%% TODO
		true ->
			%% Execute program
			conductor_response:create_program(Request),
			Program:execute(Request)
	end.

%% ============================================================================
% @doc Execute a model file during the execution of a program
% @spec
execute_model(ModelFile, Function, Arguments) ->
	Request = conductor_response:get_request(),
	Domain = wrq:domain_tokens(Request),

	ModelRoot = conductor_settings:get(Domain, model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),
	
	case conductor_cache:get_module(ModelPath) of
		false ->
			%% TODO
		Model ->
			execute_model_module(Request, Model, Function, Arguments)
	end.

execute_model_module(Request, Model, Function, Arguments) ->
	case erlang:function_exported(Model, Function, 2) of
		false ->
			%% TODO
		true ->
			Model:Function(Request, Arguments)
	end.

%% ============================================================================
% @doc Render a view file during the execution of a program
% @spec
execute_view(ViewFile, Arguments) ->
	Request = conductor_response:get_request(Request),
	Domain = wrq:domain_tokens(Request),

	ViewRoot = conductor_settings:get(Domain, view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),
	
	case conductor_cache:get_module(ViewPath) of
		false ->
			%% TODO
		View ->
			get_view_template(View, Arguments)
	end.

get_view_template(View, Arguments) ->
	case erlang:function_exported(View, get, 0) of
		false ->
			%% TODO
		true ->
			%% Get view compiler and template
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
	Request = conductor_response:get_request(Request),
	Domain = wrq:domain_tokens(Request),

	ControllerRoot = conductor_settings:get(Domain, controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),
	
	case conductor_cache:get_module(ControllerPath) of
		false ->
			%% TODO
		Controller ->
			execute_controller_module(Request, Controller, Function, Arguments)
	end.

execute_controller_module(Request, Controller, Function, Arguments) ->
	case erlang:function_exported(Controller, Function, 2) of
		false ->
			%% TODO
		true ->
			Controller:Function(Request, Arguments)
	end.

%% ============================================================================
%% Logging functions
%% ============================================================================

log_compiler_error(error) ->
	lager:warning("Compiler not specified in template");
log_compiler_error(Compiler) ->
	lager:warning("Could not find the render function in ~s", [Compiler]).

log_render_content_error(Errors) ->
	lager:warning("Could not render template ~p", [Errors]).

log_render_api_error(Compiler) ->
	lager:warning("Compiler ~s returned invalid value", [Compiler]).
