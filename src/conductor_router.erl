-module(conductor_router).

-export([
	execute/2,
	execute_model/3,
	execute_view/2,
	execute_controller/3
]).

-include_lib("webmachine/include/webmachine.hrl").

execute(Request,Response) ->
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
					conductor_response:create_program(Response),
					conductor_response:set_status_code(Response, 404),
					execute_error(Request,Response);
				true ->
					%% Create file response
					conductor_response:create_file(Response),
					conductor_response:add_content(Response, FilePath)
			end;
		{ProgramName, ProgramFile} ->
			%% Create program response
			conductor_response:create_program(),
			execute_program(ProgramFile, Request,Response)

	end.

%% ----------------------------------------------------------------------------
% @spec get_cookies(CookieHeader::string) -> Cookies::Tuplelist()
% @doc Get cookies from request
%% ----------------------------------------------------------------------------
get_cookies(CookieHeader) ->
	get_cookies(string:tokens(CookieHeader, ";"), []).

get_cookies([], Cookies) ->
	Cookies;
get_cookies([Data | Rest], Cookies) ->
	case string:tokens(Data, "=") of 
		[Name, Value] ->
			NewCookie = {string:strip(Name), string:strip(Value)},
			get_cookies(Rest, [NewCookie | Cookies]);
		_ ->
			get_cookies(Rest, Cookies)
	end.

%% ----------------------------------------------------------------------------
% @spec execute_program(ProgramFile, Parameters) -> ok
% @doc Execute a program
%% ----------------------------------------------------------------------------
execute_program(ProgramFile, Request,Response) ->
	%% Get the program module from cache
	case conductor_cache:get_program(ProgramFile) of
		false ->
			%% Program file does not exist
			%% Create "410 Gone" response
			conductor_response:set_status_code(410),
			execute_error(Request,Response);
		Program ->
			case erlang:function_exported(Program, execute, 2) of
				false ->
					%% Program file is missing the called function 
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500),
					%% TODO: Create response body
				true ->
					%% Execute program
					Program:execute(Request,Response)
			end
	end.

execute_error(Request,Response) ->
	case lists:keyfind(error, 1, conductor_settings:get(programs)) of
		false ->
			%% 'error' is not specified in configuration 
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response body
		{error, ProgramFile} ->
			execute_program(ProgramFile, Request,Response)
	end;

%% ----------------------------------------------------------------------------
% @spec execute_model(ModelFile, Function, Arguments, Parameters, Log)
% @doc Get cookies from request
%% ----------------------------------------------------------------------------
execute_model(ModelFile,Function,Arguments, Request,Response) ->
	case conductor_cache:get_model(ModelFile) of
		false ->
			%% Model file does not exist
			%% TODO: Write to log file
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response body
		Model ->
			case erlang:function_exported(Model, Function, 3) of
				false ->
					%% Model file 
					%% TODO: Write to log file
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response body
				true ->
					%% Execute model
					Model:Function(Arguments, Request,Response)
			end
	end.

%% ----------------------------------------------------------------------------
% @spec get_cookies(CookieHeader::string) -> Cookies::Tuplelist()
% @doc Get cookies from request
%% ----------------------------------------------------------------------------
execute_view(ViewFile, Arguments) ->
	case conductor_cache:get_view(ViewFile) of
		false ->
			%% View file does not exist
			%% TODO: Write to log file
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response body
		View ->
			case erlang:function_exported(View, render, 1) of
				false ->
					%% View file code contains errors
					%% TODO: Write to log file
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response body
				true ->
					%% Execute view
					View:render(Arguments)
			end
	end.

%% ----------------------------------------------------------------------------
% @spec get_cookies(CookieHeader::string) -> Cookies::Tuplelist()
% @doc Get cookies from request
%% ----------------------------------------------------------------------------
execute_controller(ControllerFile,Function,Arguments, Request,Response)  ->
	case conductor_cache:get_controller(ControllerFile) of
		false ->
			%% Controller file does not exist
			%% TODO: Write to log file
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
			%% TODO: Create response body
		Controller ->
			case erlang:function_exported(Controller, Function, 3) of
				false ->
					%% Controller code contains errors 
					%% TODO: Write to log file
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
					%% TODO: Create response body
				true ->
					%% Execute controller
					Controller:Function(Arguments, Request,Response)
			end
	end.

