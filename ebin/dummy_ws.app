{application, 'dummy_ws', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['dummy_ws_app','dummy_ws_sup']},
	{registered, [dummy_ws_sup]},
	{applications, [kernel,stdlib]},
	{mod, {dummy_ws_app, []}},
	{env, []}
]}.