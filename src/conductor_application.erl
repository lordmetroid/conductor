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
	HostTokens = wrq:host_tokens(Request),
	Domain = create_domain(HostTokens),

	case conductor_settings:get(Domain, programs) of
		undefined ->
			log_no_programs_error(Domain),
			false;
		Programs ->
			select_response_type(Request, Domain, Path, Programs)
	end.
			
select_response_type(Request, Domain, Path, Programs) ->
	case lists:keyfind(Path, 1, Programs) of
		false ->
			publish_file(Request, Domain);
		{Path, ProgramFile} ->
			get_program_module(Request, Domain, ProgramFile)
	end.

publish_file(Request, Domain) ->
	Path = wrq:disp_path(Request),
	FileRoot = conductor_settings:get(Domain, file_root),
	FilePath = filename:join([FileRoot, Path]),

	case filelib:is_regular(FilePath) of
		false ->
			false; %% TODO: 404 Not Found
		true ->
			create_file_response(Request, FilePath)
	end.

create_file_response(Request, FilePath) ->
	case conductor_response:create_file(Request) of
		error ->
			false;
		ok ->
			conductor_response:add_data(FilePath)
	end.

get_program_module(Request, Domain, ProgramFile) ->
	ProgramRoot = conductor_settings:get(Domain, program_root),
	ProgramPath = filename:join([ProgramRoot, ProgramFile]),

	case conductor_cache:get_module(program, ProgramPath) of
		false ->
			log_module_not_cached_error(ProgramPath),
			false;
		Program ->
			execute_program_module(Request, Program)
	end.

execute_program_module(Request, Program) ->
	case erlang:function_exported(Program, execute, 1) of
		false ->
			log_function_not_exported_error(execute, 1),
			false;
		true ->
			create_program_response(Request, Program)
	end.

create_program_response(Request, Program) ->
	case conductor_response:create_program(Request) of
		error ->
			false;
		ok ->
			Data = Program:execute(Request),
			conductor_response:add_data(Data)
	end.


%% ============================================================================
% @doc Execute a model file during the execution of a program
% @spec
execute_model(ModelFile, Function, Arguments) ->
	case conductor_response:get_request() of
		false ->
			error;
		Request ->
			get_model_module(Request, ModelFile, Function, Arguments)
	end.

get_model_module(Request, ModelFile, Function, Arguments) ->
	HostTokens = wrq:host_tokens(Request),
	Domain = create_domain(HostTokens),

	ModelRoot = conductor_settings:get(Domain, model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),
	
	case conductor_cache:get_module(model, ModelPath) of
		false ->
			log_module_not_cached_error(ModelPath),
			error;
		Model ->
			execute_model_module(Request, Model, Function, Arguments)
	end.

execute_model_module(Request, Model, Function, Arguments) ->
	case erlang:function_exported(Model, Function, 2) of
		false ->
			log_function_not_exported_error(Function, 2),
			error;
		true ->
			Model:Function(Request, Arguments)
	end.

%% ============================================================================
% @doc Render a view file during the execution of a program
% @spec
execute_view(ViewFile, Arguments) ->
	case conductor_response:get_request() of
		false ->
			[];
		Request ->
			get_view_module(Request, ViewFile, Arguments)
	end.

get_view_module(Request, ViewFile, Arguments) ->
	HostTokens = wrq:host_tokens(Request),
	Domain = create_domain(HostTokens),

	ViewRoot = conductor_settings:get(Domain, view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),
	
	case conductor_cache:get_module(view, ViewPath) of
		false ->
			log_module_not_cached_error(ViewPath),
			[];
		View ->
			get_view_template(ViewPath, View, Arguments)
	end.

get_view_template(ViewPath, View, Arguments) ->
	case erlang:function_exported(View, get, 0) of
		false ->
			log_function_not_exported_error(get, 0),
			[];
		true ->
			{Compiler, Template} = View:get(),
			validate_template(ViewPath, Compiler, Template, Arguments)
	end.

validate_template(ViewPath, Compiler, Template, Arguments) ->
	case erlang:function_exported(Compiler, render, 2) of
		false ->
			log_compiler_error(Compiler),
			[];
		true ->
			render_view_template(ViewPath, Compiler, Template, Arguments)
	end.

render_view_template(ViewPath, Compiler, Template, Arguments) ->
	case Compiler:render(Template, Arguments) of
		{error, Content, Errors} ->
			log_render_data_error(ViewPath, Errors),
			Content;
		{ok, Content} ->
			Content;
		_ ->
			log_render_api_error(Compiler),
			[]
	end.

%% ============================================================================
%%  @doc Execute a controller file during the execution of a program
%%
execute_controller(ControllerFile, Function, Arguments)  ->
	case conductor_response:get_request() of
		false ->
			[];
		Request ->
			get_controller_module(Request, ControllerFile, Function, Arguments)
	end.

get_controller_module(Request, ControllerFile, Function, Arguments) ->
	HostTokens = wrq:host_tokens(Request),
	Domain = create_domain(HostTokens),

	ControllerRoot = conductor_settings:get(Domain, controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),
	
	case conductor_cache:get_module(controller, ControllerPath) of
		false ->
			log_module_not_cached_error(ControllerPath),
			[];
		Controller ->
			execute_controller_module(Request, Controller, Function, Arguments)
	end.

execute_controller_module(Request, Controller, Function, Arguments) ->
	case erlang:function_exported(Controller, Function, 2) of
		false ->
			log_function_not_exported_error(Function, 2),
			[];
		true ->
			Controller:Function(Request, Arguments)
	end.

%% ============================================================================
%% Helper function
%% ============================================================================

%% @doc Join domain tokens together
%% @spec
create_domain([Token | Rest]) ->
	lists:flatten([Token] ++ [ "." ++ X || X <- Rest]).


%% ============================================================================
%% Logging functions
%% ============================================================================

log_no_programs_error(Domain) ->
	lager:warning("No programs specified for: ~s", [Domain]).

log_module_not_cached_error(Path) ->
	lager:warning("Module ~s is not cached", [Path]).

log_function_not_exported_error(Function, Arity) ->
	lager:warning("Could not find ~s/~B", [Function, Arity]).

log_compiler_error(error) ->
	lager:warning("Compiler not specified in template");
log_compiler_error(Compiler) ->
	lager:warning("Could not find the render function in ~s", [Compiler]).

log_render_data_error(ViewPath, []) ->
	lager:warning("Template ~s was not rendered correctly", [ViewPath]);
log_render_data_error(ViewPath, [Error | Rest]) ->
	lager:warning("Template rendering error: ~s", [Error]),
	log_render_data_error(ViewPath, Rest).

log_render_api_error(Compiler) ->
	lager:warning("Compiler ~s returned invalid value", [Compiler]).

