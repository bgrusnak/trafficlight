%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(trafficlight_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/clear", clear_handler, []},
			{"/sequence/create", sq_create_handler, []},
			{"/observation/add", ob_add_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),

	trafficlight_sup:start_link().

stop(_State) ->
	ok.
