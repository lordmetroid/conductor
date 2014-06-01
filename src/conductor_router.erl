-module(conductor_router).

-export([
	execute/2
]).

-include_lib("webmachine/include/webmachine.hrl").

execute(Request, Response) ->
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
					%% TODO: Create 404 response
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
					%% Resource is gone!
					%% TODO: Create 410 response

				Program ->
					Program:execute(Parameters, Response)
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
