-module(conductor_router).

-export([
	execute/2
]).

-include_lib("webmachine/include/webmachine.hrl").

execute(Request, Response) ->
	%% Find a matching response to the request
	Path = wrq:path(Request),
	case proplists:get_value(Path, conductor_settings:get(programs)) of
		%% Request is not a program
		undefined ->
			%% Check if request is a regular file
			Filename = filename:join(conductor_settings:get(file_path), Path),
			case filelib:is_regular(Filename) of
				false ->
					%% Request not found
					%% TODO: Create 404 response
					undefined;
				true ->
					%% Create a file response
					conductor_response:create(Response, file),
			
					%% Add file content to the response
					conductor_response:add_content(Filename)
			end;
		ProgramName ->
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
			ProgramModule = conductor_cache:get_program(ProgramName),
			ProgramModule:execute(Parameters, Response)
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
