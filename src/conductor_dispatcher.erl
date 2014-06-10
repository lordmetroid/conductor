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
	%% Initialize a response session
	conductor_response:start_session(),

service_available(Request,Response) ->
	%% Exceute request
	conductor_router:execute(Request),

	%% Check HTTP Status Code
	case conductor_response:get_status_code() of
		500 ->
			%% "500 Internal Server Error"
			%% TODO: Print session log as error term
			{{error, []}, Request,Response};
		503 ->
			%% "503 Service Unavailable"
			{false, Request,Response};
		_ ->
			%% Requested service is available
			{true, Request,Response}
	end.

resource_exists(Request,Response) ->
	case conductor_response:get_status_code() of
		404 ->
			%% "404 File Not Found" 
			{false, Request,Response};
		_ ->
			%% Requested file was found
			{true, Request,Response}
	end.

options(Request,Response) ->
	%% Set custom response headers
	{[], Request,Response}.

content_types_provided(Request,Response) ->
	%% Set response mimetype
	MimeType = conductor_response:get_mime_type(),
	{[{MimeType, provide_content}], Request,Response}.

%% ----------------------------------------------------------------------------
% @spec provide_content(Request,Response) -> {Body::iolist(), Request,Response}
% @doc Provide the response body to the client
%% ----------------------------------------------------------------------------
provide_content(Request,Response) ->
	%%	Publish content to client
	Content = conductor_response:get_content(),

	conductor_response:end_session(),
	{Content, Request,Response}.

