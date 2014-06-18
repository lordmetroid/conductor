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
		{error, Errors} ->
			%% Program file does not exist
			%% Create "410 Gone" response
			conductor_response:set_status_code(410),
			execute_error(Request);
		{error, Errors, Warnings} ->

		{ok, Model, Warnings} ->

		{ok, Model} ->
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
			%% 'error' is not specified in configuration 
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
	case conductor_cache:get_model(ModelFile) of
		false ->
			%% Model file does not exist
			%% TODO: Write to log file
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		Model ->
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
	case conductor_cache:get_view(ViewFile) of
		false ->
			%% View file does not exist
			%% TODO: Write to log file
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		View ->
			case erlang:function_exported(View, get, 0) of
				false ->
					%% View file code contains errors
					%% TODO: Write to log file
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response term
				true ->
					%% Get view compiler and template
					{Compiler, Template} = View:get(),
					render_view(Compiler, Template, Arguments)
			end
	end.

%% ----------------------------------------------------------------------------
% @spec render_view(Compiler, Template, Arguments) -> 
% @doc Add template content to response body
%% ----------------------------------------------------------------------------
render_view(Compiler, [], Arguments) ->
	ok;
render_view(Compiler, [Token | Rest], Arguments) ->
	%% Let the compiler render the token
	case Compiler:render(Token, Arguments) of
		{error, Errors} ->
			%% TODO: Log error
			
		{ok, Content} ->
			%% Add rendered content to response body
			conductor_response:add_content(Content),
			render_view(Compiler, Rest, Arguments)
	end.

%% ----------------------------------------------------------------------------
% @spec execute_controller(ControllerFile,Function, Arguments, Request)
% @doc
%% ----------------------------------------------------------------------------
execute_controller(ControllerFile,Function,Arguments, Request)  ->
	case conductor_cache:get_controller(ControllerFile) of
		false ->
			%% Controller file does not exist
			%% TODO: Write to log file
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response term
		Controller ->
			case erlang:function_exported(Controller, Function, 2) of
				false ->
					%% Controller code contains errors 
					%% TODO: Write to log file
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response term
				true ->
					%% Execute controller
					Controller:Function(Request, Arguments)
			end
	end.
