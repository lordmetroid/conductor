{application, conductor, [
	{description,"Conductor web application platform"},
	{vsn,"1"},

	{applications, [
		kernel,
		stdlib,

		webmachine,
		lager,
		mimetypes,
		uuid,
		dynamic_compile,

		symphony
	]},

	{modules,[
		conductor_application,
		conductor_application_supervisor,
		conductor_cache,
		conductor_cache_compiler,
		conductor_response,
		conductor_response_body,
		conductor_response_header,
		conductor_settings,
		conductor_supervisor,
		conductor_web_interface,
		conductor_web_interface_router,
		conductor_web_interface_supervisor
	]},
	{registered, [
		conductor_application_supervisor,
		conductor_cache,
		conductor_response,
		conductor_response_body,
		conductor_response_header,
		conductor_settings,
		conductor_supervisor,
		conductor_web_interface_supervisor
	]},

	{mod, {conductor_application, []}}
]}.
