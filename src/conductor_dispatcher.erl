-module(conductor_dispatcher).

-export([
	init/1,
	service_available/2,
	options/2,
	content_types_provided/2,
	provide_content/2
]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Configurations) ->
	%% Initialize a response
	conductor_response:start().

service_available(Request, Response) ->
	%% Compile response by exceuting request
	conductor_router:execute(Request, Response),
	{true, Request, Response}.

options(Request, Response) ->
	%% Set custom response headers
	{[], Request, Response}.

content_types_provided(Request, Response) ->
	%% Set response mimetype
	MimeType = conductor_response:get_mime_type(Response),
	{[{MimeType, provide_content}], Request, Response}.

provide_content(Request, Response) ->
	%%	Publish content to requesting client
	{conductor_response:get_content(Response), Request, Response}.
%   {["WORKS: ", wrq:path(Request)], Request, Response}.
