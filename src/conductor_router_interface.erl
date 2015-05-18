-module(conductor_router_interface).
-compile({parse_transform, lager_transform}).

-export([
	init/1,
	service_available/2,
	allowed_methods/2,
	resource_exists/2,

	content_types_provided/2,
	get_resource/2
]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Arguments) ->
	{ok, []}.

service_available(Request, Context) ->
	%% Execute the request to create a response
	conductor_router:execute(Request),
	{true, Request, Context}.

allowed_methods(Request, Context) ->
	Methods = [
		'HEAD',
		'GET',
		'PUT',
		'POST',
		'DELETE'
	],
	{Methods, Request, Context}.

resource_exists(Request,Context) ->
	IsResource = conductor_response:exists(),
	{IsResource, Request, Context}.

%% ============================================================================
%% GET functions
%% ============================================================================

%% @doc Publish the results of an executed program or content of a file
content_types_provided(Request, Context) ->
	Domain = wrq:host_tokens(Request),
	Path = wrq:path(Request),

	MimeType = conductor_router:mime_type(Domain, Path),
	ContentTypes = [
		{MimeType, get_resource}
	],
	{ContentType, Request, Context}.

get_resource(Request, Context) ->
	Domain = wrq:host_tokens(Request),
	Path = wrq:path(Request),

	Content = conductor_router:execute(Domain, Path),
	{Content, Request, Context}.

%% ============================================================================
%% POST functions
%% ============================================================================





	case conductor_response:get_status_code() of
		404 ->
			%% Set response header to "404 File Not Found" 
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
	
	%% Terminate current response
	conductor_response:destroy(),
	
	%% Publish content to client
	{Content, Request,Context}.
