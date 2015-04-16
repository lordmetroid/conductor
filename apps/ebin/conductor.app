{application, conductor, [
	{description, "Conductor web application platform"},
	{vsn, "1"},
	(applications, [
		lager,
		webmachine
	]},
	{modules, [
		conductor_application,
		conductor_application_supervisor,
		conductor_cache_compiler,
		conductor_cache
		conductor_response_body
		conductor_response_header
		conductor_response
		conductor_settings
		conductor_supervisor
		conductor_web_interface
		conductor_web_interface_router
		conductor_web_interface_supervisor
	]},
	{registered, [
		conductor_application_supervisor,
		conductor_supervisor,
		conductor_web_interface_supervisor,
		conductor_cache,
		conductor_response_body,
		conductor_response_header,
		conductor_response,
		conductor_settings,
	]},
	{mod, {conductor_application, []}}
]}.

