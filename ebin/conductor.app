{application, conductor, [
	{description, "Conductor web application platform"},
	{vsn, "0.1"},
	{modules, [
		conductor,
		conductor_application,
		conductor_cache,
		conductor_compiler,
		conductor_dispatcher,
		conductor_response,
		conductor_router,
		conductor_settings,
		conductor_supervisor
	]},
	{registered, [
		conductor_settings,
		conductor_supervisor,
		conductor_system_supervisor,
		conductor_server_supervisor
	]},
	{mod, {conductor_application, []}}
]}.

