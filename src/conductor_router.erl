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
					conductor_response:add_content(FilePath)
			end;
		{ProgramName, ProgramFile} ->
			%% Create program response
			conductor_response:create_program(),
			execute_program(Request, ProgramFile)

	end.

%% ----------------------------------------------------------------------------
% @spec execute_program(ProgramFile, Request, Response) -> ok
% @doc Execute a program 
%% ----------------------------------------------------------------------------
execute_program(Request, ProgramFile) ->
	%% Get the program module from cache
	case conductor_cache:get_program(ProgramFile) of
		false ->
			%% Program file does not exist
			%% Create "410 Gone" response
			conductor_response:set_status_code(410),
			execute_error(Request);
		Program ->
			%% Check if the program is correct
			case erlang:function_exported(Program, execute, 1) of
				false ->
					%% Program is incorrect
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500),
				true ->
					%% Execute program
					Program:execute(get_parameters(Request))
			end
	end.

execute_error(Request) ->
	case lists:keyfind(error, 1, conductor_settings:get(programs)) of
		false ->
			%% Error program does not exists
			%% Create "500 Internal Server Error" response
			conductor_response:set_status_code(500);
		{error, ProgramFile} ->
			case conductor_cache:get_program(ProgramFile) of
				false ->
					%% Error program file does not exist
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
				Program ->
					case erlang:function_exported(Program, execute, 1) of
						false ->
							%% Error program is incorrect
							%% Create "500 Internal Server Error" response
							conductor_response:set_status_code(500);
						true ->
							Program:execute(get_parameters(Request))
					end
			end
	end;


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

get_parameters(Request) ->
	%% Get Request parameters
	[
		{peer, wrq:peer(Request)},
		{path, wrq:path(Request)},
		{status, conductor_response:get_status_code()},
		{method, wrq:method(Request)},
		{variables, wrq:req_qs(Request)},
		{body, wrq:req_body(Request)},
		{cookies, get_cookies(wrq:req_cookie(Request))},
		{queries, wrq:req_qs(Request)}
	].

%% ----------------------------------------------------------------------------
% @spec execute_model(ModelFile, Function, Arguments, Parameters, Log)
% @doc Get cookies from request
%% ----------------------------------------------------------------------------
execute_model(ModelFile, Function, Arguments, Parameters) ->
	case conductor_cache:get_model(ModelFile) of
		false ->
			%% Model module does not exist
			%% TODO: Write to log file
			%% TODO: "410 Gone" response???
		Model ->
			case erlang:function_exported(Model, Function, 2) of
				false ->
					%% Model file is incorrect
					%% TODO: Write to log file
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
				true ->
					%% Execute model
					Model:Function(Arguments, Parameters)
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
			%% TODO: "410 Gone" response???
		View ->
			case erlang:function_exported(View, render, 2) of
				false ->
					%% View file is incorrect
					%% TODO: Write to log file
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
				true ->
					%% Execute view
					View:render(Arguments)
			end
	end.

%% ----------------------------------------------------------------------------
% @spec get_cookies(CookieHeader::string) -> Cookies::Tuplelist()
% @doc Get cookies from request
%% ----------------------------------------------------------------------------
execute_controller(ControllerFile, Function, Arguments, Parameters)  ->
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
					%% Create "500 Internal Server Error" response
					conductor_response:set_status_code(500);
				true ->
					%% Execute controller
					Controller:Function(Arguments, Parameters)
			end
	end.

