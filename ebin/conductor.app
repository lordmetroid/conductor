{application, conductor, [
	{description, "Web application server"},
	{vsn, "1"},
	{modules, [
		conductor,
		conductor_application,
		conductor_supervisor
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
