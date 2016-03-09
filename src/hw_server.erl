-module(hw_server).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    server:start_link().

stop(_State) ->
    ok.
