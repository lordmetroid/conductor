%% -*- mode: erlang; -*-
{application, conductor, [
	{description,"Conductor web application platform"},
	{vsn,"1"},

	{applications, [
		kernel,
		stdlib,

		lager,
		webmachine,

		mimetypes,
		hackney,
		pgapp
	]},

	{modules,[
		conductor,
		conductor_supervisor,

		conductor_systems_supervisor,
		conductor_settings,
		conductor_cache,
		conductor_cache_compiler,
		conductor_response,
		conductor_response_content,

		conductor_interface_supervisor,
		conductor_router_interface,
		conductor_router
	]},
	{registered, [
		conductor_supervisor,
		conductor_systems_supervisor,
		conductor_settings,
		conductor_cache,
		conductor_response,
		conductor_response_content,
		conductor_interface_supervisor
	]},

	{mod, {conductor, []}}
]}.

