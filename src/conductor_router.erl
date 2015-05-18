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
			get_file(Domain, Path);
		{Path, ProgramFile} ->
			get_program(Request, Domain, ProgramFile).
	end.

get_file(Domain, Path) ->
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
			execute_program(Request, Program)
	end.

execute_program(Request, Program)
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
	%% Get the model module from cache
	ModelRoot = conductor_settings:get(model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),
	
	case conductor_cache:get_module(ModelPath) of
		error ->
			%% Cache unable to provide model
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{ok, Model} ->
			case erlang:function_exported(Model, Function, 2) of
				false ->
					%% Model file 
					conductor_log:add(ModelPath, 
						"Function " ++ atom_to_list(Function) ++ " not found"),
						
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response term
				true ->
					%% Execute model
					Request = conductor_response:get_request(),
					Model:Function(Request, Arguments)
			end
	end.

%% ============================================================================
% @doc Render a view file during the execution of a program
% @spec
execute_view(ViewFile, Arguments) ->
	%% Get the view module from cache
	ViewRoot = conductor_settings:get(view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),
	
	case conductor_cache:get_module(ViewPath) of
		error ->
			%% Cache unable to provide view
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{ok, View} ->
			case erlang:function_exported(View, get, 0) of
				false ->
					%% View is not compiled
					conductor_log:add(ViewPath,
						"Unable to execute uncompiled view"),
					
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response term
				true ->
					%% Get view compiler and template
					{Compiler, Template} = View:get(),

					%% Render template
					case erlang:function_exported(Compiler, render, 2) of
						false ->
							%% Undefined view compiler specified
							case Compiler of
								error ->
									%% No compiler sepcified in template
									conductor_log:add(ViewPath,
										"No compiler specified in template");
								_ ->
									%% View compiler does not comply to API
									conductor_log:add(atom_to_list(Compiler),
										"Function render not found")
							end;
						true ->
							%% Render template
							case Compiler:render(Template, Arguments) of
								{error, Content, Errors} ->
									%% Log errors from rendering
									conductor_log:add(ViewFile, Errors),
									
									%% Add rendered content to response body
									conductor_response:add_content(Content);
								{ok, Content} ->
									%% Add rendered content to response body
									conductor_response:add_content(Content);									
								_ ->
									%% Render function does not comply to API
									conductor_log:add(atom_to_list(Compiler),
										"Invalid value from function render")
							end
					end
			end
	end.

%% ----------------------------------------------------------------------------
% @spec execute_controller(ControllerFile,Function, Arguments, Request)
% @doc Execute a controller file during runtime
%% ----------------------------------------------------------------------------
execute_controller(ControllerFile, Function, Arguments)  ->
	%% Get the controller module from cache
	ControllerRoot = conductor_settings:get(controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),
	
	case conductor_cache:get_module(ControllerPath) of
		error ->
			%% Cache unable to provide controller
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{ok, Controller} ->
			case erlang:function_exported(Controller, Function, 2) of
				false ->
					%% Controller does not contain function
					conductor_log:add(ControllerPath, 
						"Function " ++ atom_to_list(Function) ++ " not found"),
					
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response term
				true ->
					%% Execute controller
					Request = conductor_response:get_request(),
					Controller:Function(Request, Arguments)
			end
	end.
