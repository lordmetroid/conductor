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
% @doc Execute a program
%% ----------------------------------------------------------------------------
execute_program(ProgramFile, Request) ->
	%% Get the program module from cache
	case conductor_cache:get_program(ProgramFile) of
		false ->
			%% Program file does not exist
			%% Create "410 Gone" response
			conductor_response:set_status_code(410),
			execute_error(Request);
		Model ->
			case erlang:function_exported(Program, execute, 1) of
				false ->
					%% Program file is missing the called function 
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500),
					%% TODO: Create response term
				true ->
					%% Execute program
					Program:execute(Request)
			end
	end.

execute_error(Request) ->
	case lists:keyfind(error_program, 1, conductor_settings:get(programs)) of
		false ->
			%% 'error_program' is not specified in configuration 
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{error_program, ProgramFile} ->
			execute_program(ProgramFile, Request)
	end;

%% ----------------------------------------------------------------------------
% @spec execute_model(ModelFile, Function, Arguments, Parameters, Log)
% @doc
%% ----------------------------------------------------------------------------
execute_model(ModelFile,Function,Arguments, Request) ->
	ModelRoot = conductor_settings:get(model_root),
	ModelPath = filename:join([ModelRoot, ModelFile]),
	
	case conductor_cache:get_model(ModelPath) of
		{error, Errors} ->
			%% Model file does not exist
			conductor_log:add(Errors),
			
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{ok, Model} ->
			case erlang:function_exported(Model, Function, 2) of
				false ->
					%% Model file 
					%% TODO: Write to log file
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
% @doc
%% ----------------------------------------------------------------------------
execute_view(ViewFile, Arguments) ->
	ViewRoot = conductor_settings:get(view_root),
	ViewPath = filename:join([ViewRoot, ViewFile]),
	
	case conductor_cache:get_view(ViewPath) of
		{error, Errors} ->
			%% View file does not exist
			conductor_log:add(Errors),
			
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{ok, View} ->
			case erlang:function_exported(View, get, 0) of
				false ->
					%% View file code contains errors
					conductor_log:add(ViewFile ++ " - could not executed"),
					
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
									conductor_log:add("No compiler specified in template"),
								_ ->
									conductor_log:add(Compiler ++ " does not contain render function")
							end;
						true ->
							%% Render template
							{Content, Errors} = Compiler:render(Token, Arguments),
							
							%% Log errors from rendering
							conductor_log:add(ViewFile, Errors),
							
							%% Add rendered content to response body
							conductor_response:add_content(Content)
					end.
			end
	end.

%% ----------------------------------------------------------------------------
% @spec execute_controller(ControllerFile,Function, Arguments, Request)
% @doc
%% ----------------------------------------------------------------------------
execute_controller(ControllerFile,Function,Arguments, Request)  ->
	case conductor_cache:get_controller(ControllerFile) of
		{error, Errors} ->
			%% Cache could not provide ControllerFile
			conductor_log:add(Errors),
			
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		{ok, Controller} ->
			case erlang:function_exported(Controller, Function, 2) of
				false ->
					%% Controller code contains errors 
					conductor_log:add(ControllerFile ++ " - does not contain " ++ Function),
					
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response term
				true ->
					%% Execute controller
					Controller:Function(Request, Arguments)
			end
	end.
