-module(conductor_router).

-export([
	execute/2
]).

-include_lib("webmachine/include/webmachine.hrl").

execute(Request, Response) ->
	%% Find a match response to the request
	Path = wrq:path(Request),
	case proplists:get_value(Path, conductor_settings:get(programs)) of
		undefined ->
			%% TODO: Check if request is a file
			conductor_response:create(Response, file),


			%% TODO: Read file
			undefined; 
		Program ->
			%% Create a program response
			conductor_response:create(Response, program),

			%% Get the program parameters
			Parameters = {
				Peer = wrq:peer(Request),
				Method = wrq:method(Request),
				Path,
				Variables = wrq:req_qs(Request),
				Cookies = get_cookies(wrq:req_cookie(Request))
			}

			%% Execute the program
%%			conductor_system:execute(Program, Parameters, Response)
	end.

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

