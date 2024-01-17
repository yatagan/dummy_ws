-module(dummy_ws_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, dummy_ws, "index.html"}},
			{"/socket", leaky_ws, []},
			{"/static/[...]", cowboy_static, {priv_dir, dummy_ws, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	dummy_ws_sup:start_link().

stop(_State) ->
	ok.
