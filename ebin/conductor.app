{application, conductor, [
	{description, "Web application server"},
	{vsn, "1"},
	{modules, [
		conductor,
		conductor_application,
		conductor_supervisor,
		conductor_settings,
		conductor_response,
		conductor_dispatcher,
		conductor_router
	]},
	{registered, [
		conductor_supervisor,
		conductor_system_supervisor,
		conductor_server_supervisor,
		conductor_settings

	]},
	{applications, [
		kernel,
		stdlib,
		sasl,
		mochiweb,
		webmachine
	]},
	{mod, {conductor_application, []}}
]}.
