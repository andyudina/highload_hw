-module(app).
-export([start_link/0]).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    server:start_link().

stop(_State) ->
    ok.
