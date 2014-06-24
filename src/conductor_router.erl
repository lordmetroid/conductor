-module(conductor_router).

-export([
	execute/2,
	execute_model/3,
	execute_view/2,
	execute_controller/3
]).

-include_lib("webmachine/include/webmachine.hrl").

execute(Request) ->
	%% Find a matching response to the request
	ProgramName = wrq:path(Request),
	case lists:keyfind(ProgramName, 1, conductor_settings:get(programs)) of
		false ->
			%% Program not found
			FileRoot = conductor_settings:get(file_root),
			FilePath = filename:join([FileRoot, ProgramName]),

			%% Check if request is a regular file
			case filelib:is_regular(FilePath) of
				false ->
					%% Create "404 File not found" response
					conductor_response:create_program(),
					conductor_response:set_status_code(404),
					execute_error(Request);
				true ->
					%% Create file response
					conductor_response:create_file(),
					conductor_response:add_content(FilePath),
					conductor_response:set_mime_type(FilePath)
			end;
		{ProgramName, ProgramFile} ->
			%% Create program response
			conductor_response:create_program(),
			execute_program(ProgramFile, Request)
	end.

%% ----------------------------------------------------------------------------
% @spec execute_program(ProgramFile, Parameters) -> ok
% @doc Execute a program file during runtime
%% ----------------------------------------------------------------------------
execute_program(ProgramFile, Request) ->
	%% Get the program module from cache
	ProgramRoot = conductor_settings:get(program_root),
	ProgramPath = filename:join([ProgramRoot, ProgramFile]),

	case conductor_cache:get_program(ProgramPath) of
		false ->
			%% Cache unable to provide program
			conductor_log:add(ProgramPath, Errors),
			
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		Model ->
			case erlang:function_exported(Program, execute, 1) of
				false ->
					%% Program file is missing the execute function
					conductor_log:add(ProgramPath, 
						"Function execute not found"),
					
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500),
					%% TODO: Create response term
				true ->
					%% Execute program
					Program:execute(Request)
			end
	end.

%% ----------------------------------------------------------------------------
% @spec execute_program(ProgramFile, Parameters) -> ok
% @doc Execute an error program during runtime
%% ----------------------------------------------------------------------------
execute_error(Request) ->
	case lists:keyfind(error_program, 1, conductor_settings:get(programs)) of
		false ->
			%% 'error_program' is not specified in configuration 
			conductor_log:add("Server configuration file",
				"error_program not specified"),
			
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{error_program, ProgramFile} ->
			execute_program(ProgramFile, Request)
	end;

%% ----------------------------------------------------------------------------
% @spec execute_model(ModelFile, Function, Arguments, Parameters, Log)
% @doc Execute a model file during runtime
%% ----------------------------------------------------------------------------
execute_model(ModelFile,Function,Arguments, Request) ->
	%% Get the model module from cache
	ModelRoot = conductor_settings:get(model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),
	
	case conductor_cache:get_model(ModelPath) of
		{error, Errors} ->
			%% Cache unable to provide model
			conductor_log:add(ModelPath, Errors),
			
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
					Model:Function(Request, Arguments)
			end
	end.

%% ----------------------------------------------------------------------------
% @spec execute_view(ViewFile, Arguments)
% @doc Render a view file
%% ----------------------------------------------------------------------------
execute_view(ViewFile, Arguments) ->
	%% Get the view module from cache
	ViewRoot = conductor_settings:get(view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),
	
	case conductor_cache:get_view(ViewPath) of
		{error, Errors} ->
			%% Cache unable to provide view
			conductor_log:add(Errors),
			
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
										"No compiler specified in template"),
								_ ->
									%% View compiler does not comply to API
									conductor_log:add(atom_to_list(Compiler),
										"Function render not found")
							end;
						true ->
							%% Render template
							case Compiler:render(Template, Arguments) of
								{Content, Errors} ->
									%% Log errors from rendering
									conductor_log:add(ViewFile, Errors),
									
									%% Add rendered content to response body
									conductor_response:add_content(Content)
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
execute_controller(ControllerFile,Function,Arguments, Request)  ->
	%% Get the controller module from cache
	ControllerRoot = conductor_settings:get(controller_root),
	ControllerPath = filename:join([ControllerRoot, ControllerFile]),
	
	case conductor_cache:get_controller(ControllerPath) of
		{error, Errors} ->
			%% Cache unable to provide controller
			conductor_log:add(ControllerPath, Errors),
			
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
					Controller:Function(Request, Arguments)
			end
	end.
