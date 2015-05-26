-module(conductor_application_interface).
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
	conductor_application:execute(Request),

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
	case conductor_response:get_mime_type() of
		false ->
			{[], Request, Context};
		MimeType ->
			{[{MimeType, get_resource}], Request, Context}
	end.

get_resource(Request, Context) ->
	Data = conductor_response:get_data(),
	{Data, Request, Context}.

%% ============================================================================
%% POST functions
%% ============================================================================


