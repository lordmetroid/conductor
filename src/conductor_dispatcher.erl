-module(conductor_dispatcher).

-export([
	init/1,
	service_available/2,
	resource_exists/2,
	options/2,
	content_types_provided/2,
	provide_content/2
]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Configurations) ->
	%% No context
	{ok, []}.

service_available(Request,Context) ->
	%% Exceute request
	conductor_router:execute(Request),

	%% Check HTTP Status Code
	case conductor_response:get_status_code() of
		500 ->
			%% "500 Internal Server Error"
			%% TODO: Print session log as error term
			{{error, []}, Request,Context};
		503 ->
			%% "503 Service Unavailable"
			{false, Request,Context};
		_ ->
			%% Requested service is available
			{true, Request,Context}
	end.

resource_exists(Request,Context) ->
	case conductor_response:get_status_code() of
		404 ->
			%% "404 File Not Found" 
			{false, Request,Context};
		_ ->
			%% Requested file was found
			{true, Request,Context}
	end.

options(Request,Context) ->
	%% Set custom response headers
	{[], Request,Context}.

content_types_provided(Request,Context) ->
	%% Set response mimetype
	MimeType = conductor_response:get_mime_type(),
	{[{MimeType, provide_content}], Request,Context}.

%% ----------------------------------------------------------------------------
% @spec provide_content(Request,Context) -> {Body::iolist(), Request,Context}
% @doc Provide the response body to the client
%% ----------------------------------------------------------------------------
provide_content(Request,Context) ->
	%% Get response body content
	Content = conductor_response:get_content(),
	
	%% Cleanup current response session
	conductor_response:destroy(),
	
	%%	Publish content to client
	{Content, Request,Context}.
