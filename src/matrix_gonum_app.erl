-module(matrix_gonum_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    os:cmd("./priv/bin/gonum -name gonum@localhost -cookie "++atom_to_list(erlang:get_cookie())++" > /tmp/gonum.log").

stop(_State) ->
    ok.



