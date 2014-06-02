-module(conductor_router).

-export([
	execute_program/2,
	execute_model/3,
	execute_view/2,
	execute_controller/3
]).

-include_lib("webmachine/include/webmachine.hrl").

execute_program(Request, Response) ->
	%% Find a matching response to the request
	Path = wrq:path(Request),
	case lists:keyfind(Path, 1, conductor_settings:get(programs)) of
		%% Request is not a program
		false ->
			%% Check if request is a regular file
			FilePath = filename:join(conductor_settings:get(file_root), Path),
			case filelib:is_regular(FilePath) of
				false ->
					%% Request not found
					%% TODO: Create "404 Not Found" response
					undefined;
				true ->
					%% Create a file response
					conductor_response:create(Response, file),
			
					%% Add file content to the response
					conductor_response:add_content(FilePath)
			end;
		{Path, ProgramFile} ->
			%% Create a program response
			conductor_response:create(Response, program),

			%% Get the program parameters
			Parameters = {
				Peer = wrq:peer(Request),
				Method = wrq:method(Request),
				Path,
				Variables = wrq:req_qs(Request),
				Cookies = get_cookies(wrq:req_cookie(Request))
			},

			%% Execute the program
			case conductor_cache:get_program(ProgramFile) of
				false ->
					%% Program file does not exist
					%% TODO: Create "410 Gone" response
					%% TODO: Write to log file
				Program ->
					case erlang:function_exported(Program, execute, 2) of
						false ->
							%% Program file is incorrect
							%% TODO: Create "500 Internal Server Error" response
							%% TODO: Write to log file
						true ->
							%% Execute program
							Program:execute(Parameters, Response)
					end
			end
	end.

execute_model(ModelFile, Function, Arguments) ->
	case conductor_cache:get_model(ModelFile) of
		false ->
			%% Model file does not exist
			%% TODO: Create "410 Gone" response
			%% TODO: Write to log file
		Model ->
			case erlang:function_exported(Model, Function, 1) of
				false ->
					%% Model file is incorrect
					%% TODO: Create "500 Internal Server Error" response
					%% TODO: Write to log file
				true ->
					%% Execute model
					Model:Function(Arguments)
			end
	end.

execute_view(ViewFile, Arguments, Response) ->
	case conductor_cache:get_view(ViewFile) of
		false ->
			%% View file does not exist
			%% TODO: Create "410 Gone" response
			%% TODO: Write to log file
		View ->
			case erlang:function_exported(View, render, 2) of
				false ->
					%% View file is incorrect
					%% TODO: Create "500 Internal Server Error" response
					%% TODO: Write to log file
				true ->
					%% Execute view
					View:render(Arguments, Response)
			end
	end.

execute_controller(ControllerFile, Function, Arguments, Response) ->
	case conductor_cache:get_controller(ControllerFile) of
		false ->
			%% Controller file does not exist
			%% TODO: Create "410 Gone" response
			%% TODO: Write to log file
		Controller ->
			case erlang:function_exported(Controller, Function, 2) of
				false ->
					%% Controller file is incorrect
					%% TODO: Create "500 Internal Server Error" response
					%% TODO: Write to log file
				true ->
					%% Execute controller
					Controller:Function(Arguments, Response)
			end
	end.
	
%% ----------------------------------------------------------------------------
% @spec
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
