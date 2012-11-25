{application, conductor, [
	{description, "Web application server"},
	{vsn, "1"},
	{modules, [
		conductor
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
