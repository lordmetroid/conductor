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
	%% Initialize a session
	conductor_session:create(),



	Response = conductor_response:start(),
	
	%% Create a session log
	Log = conductor_log:create(),
	
	%% Pass Response and Log as session context
	{Response, Log}.

service_available(Request, {Response, Log}) ->
	%% Exceute request
	conductor_router:execute(Request, Response, Log),

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

resource_exists(Request, {Response, Log}) ->
	case conductor_response:get_status(Response) of
		404 ->
			%% Requested resource does not exists
			{false, Request, Response};
		_ ->
			%% Requested resource is present
			{true, Request, response}
	end.

options(Request, {Response, Log}) ->
	%% Set custom response headers
	{[], Request, Response}.

content_types_provided(Request, {Response, Log}) ->
	%% Set response mimetype
	MimeType = conductor_response:get_mime_type(Response),
	{[{MimeType, provide_content}], Request, Response}.

%% ----------------------------------------------------------------------------
% @spec provide_content(Request, Response) -> Body::iolist()
% @doc Provides the body of the response
%% ----------------------------------------------------------------------------
provide_content(Request, {Response, Log}) ->
	%%	Publish content to requesting client
	{conductor_response:get_content(Response), Request, Response}.
