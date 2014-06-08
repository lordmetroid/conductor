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
	%% Initialize a response
	conductor_response:start().

service_available(Request, Response) ->
	%% Exceute request
	conductor_router:execute(Request, Response),

	%% Check HTTP Status Code
	case conductor_response:get_status(Response) of
		500 ->
			%% "500 Internal Server Error"
			{{error, conductor_log:get(Log)}, Request, Response};
		503 ->
			%% "503 Service Unavailable"
			{false, Request, Response};
		_ ->
			{true, Request, Response}
	end.

resource_exists(Request, Response) ->
	case conductor_response:get_status(Response) of
		404 ->
			%% Requested resource does not exists
			{false, Request, Response};
		_ ->
			%% Requested resource is present
			{true, Request, response}
	end.

options(Request, Response) ->
	%% Set custom response headers
	{[], Request, Response}.

content_types_provided(Request, Response) ->
	%% Set response mimetype
	MimeType = conductor_response:get_mime_type(Response),
	{[{MimeType, provide_content}], Request, Response}.

%% ----------------------------------------------------------------------------
% @spec provide_content(Request, Response) -> Body::iolist()
% @doc Provides the body of the response
%% ----------------------------------------------------------------------------
provide_content(Request, Response) ->
	%%	Publish content to requesting client
	{conductor_response:get_content(Response), Request, Response}.
