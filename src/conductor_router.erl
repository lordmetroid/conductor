-module(conductor_router).

-export([
	execute/2,
	execute_model/3,
	execute_view/2,
	execute_controller/3
]).

-include_lib("webmachine/include/webmachine.hrl").

execute(Request, Response) ->
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
					%% 404 File not found
					execute_error(404, Request, Response);
				true ->
					%% Create a file response
					conductor_response:create(Response, file),
					conductor_response:add_content(FilePath)
			end;
		{ProgramName, ProgramFile} ->
			%% Execute program 
			execute_program(ProgramFile, Request, Response)

	end.

execute_error(StatusCode, Request, Response) ->
	case lists:keyfind(error, 1, conductor_settings:get(programs)) of
		false ->
			%% Error program could not be found
			%% TODO: 500 Internal Server Error
		{ProgramName, ProgramFile} ->
			
			execute_program(ProgramFile, Request, Response)
	end.

%% ----------------------------------------------------------------------------
% @spec execute_program(ProgramFile, Request, Response) -> ok
% @doc Execute a program 
%% ----------------------------------------------------------------------------
execute_program(ProgramFile, Request, Response) ->
	%% Create a program response
	conductor_response:create(Response, program),

	%% Get the program parameters
	Parameters = [
		{peer, wrq:peer(Request)},
		{method, wrq:method(Request)},
		{variables, wrq:req_qs(Request)},
		{cookies, get_cookies(wrq:req_cookie(Request))}
	],

	%% Get the program module from cache
	case conductor_cache:get_program(ProgramFile) of
		false ->
			%% Program file does not exist
			%% TODO: Write to log file
			%% TODO: Create "410 Gone" response
		Program ->
			%% Check if the program is correct
			case erlang:function_exported(Program, execute, 2) of
				false ->
					%% Program is incorrect
					%% TODO: Write to log file
					%% TODO: "500 Internal Server Error"
				true ->
					%% Execute program
					Program:execute(Parameters, Response)
			end
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

execute_model(ModelFile, Function, Arguments, Parameters) ->
	case conductor_cache:get_model(ModelFile) of
		false ->
			%% Model module does not exist
			%% TODO: Write to log file
			%% TODO: "410 Gone" response
		Model ->
			case erlang:function_exported(Model, Function, 2) of
				false ->
					%% Model file is incorrect
					%% TODO: Write to log file
					%% TODO: "500 Internal Server Error" response
				true ->
					%% Execute model
					Model:Function(Arguments, Parameters)
			end
	end.

execute_view(ViewFile, Arguments, Response) ->
	case conductor_cache:get_view(ViewFile) of
		false ->
			%% View file does not exist
			%% TODO: Write to log file
			%% TODO: "410 Gone" response
		View ->
			case erlang:function_exported(View, render, 2) of
				false ->
					%% View file is incorrect
					%% TODO: Write to log file
					%% TODO: "500 Internal Server Error" response
				true ->
					%% Execute view
					View:render(Arguments, Response)
			end
	end.

execute_controller(ControllerFile, Function, Arguments,Parameters, Response) ->
	case conductor_cache:get_controller(ControllerFile) of
		false ->
			%% Controller file does not exist
			%% TODO: Write to log file
			%% TODO: "410 Gone" response
		Controller ->
			case erlang:function_exported(Controller, Function, 3) of
				false ->
					%% Controller file is incorrect
					%% TODO: Write to log file
					%% TODO: "500 Internal Server Error" response
				true ->
					%% Execute controller
					Controller:Function(Arguments, Parameters, Response)
			end
	end.
			

