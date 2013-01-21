{application, conductor, [
	{description, "Conductor web application platform"},
	{vsn, "0.1"},
	{modules, [
		conductor,
		conductor_application,
		conductor_supervisor,
		conductor_settings,
		conductor_dispatcher,
		conductor_router,
		conductor_compiler,
		conductor_response
	]},
	{registered, [
		conductor_settings,
		conductor_supervisor,
		conductor_system_supervisor,
		conductor_server_supervisor
	]},
	{mod, {conductor_application, []}}
]}.

