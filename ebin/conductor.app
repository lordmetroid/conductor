{application, conductor, [
	{description, "Conductor web application platform"},
	{vsn, "0.1"},
	{modules, [
		conductor,
		conductor_application,
		conductor_cache,
		conductor_compiler,
		conductor_dispatcher,
		conductor_log,
		conductor_response,
		conductor_response_body,
		conductor_response_header,
		conductor_router,
		conductor_settings,
		conductor_supervisor
	]},
	{registered, [
		conductor_cache,
		conductor_log,
		conductor_response,
		conductor_settings,
		conductor_supervisor,
		conductor_system_supervisor,
		conductor_server_supervisor
	]},
	{mod, {conductor_application, []}}
]}.

