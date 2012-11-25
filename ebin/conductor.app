{application, conductor, [
	{description, "Web application server"},
	{vsn, "1"},
	{modules, [
		conductor,
		conductor_application
	]},
	{registered, [
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
