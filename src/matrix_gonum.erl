-module(matrix_gonum).
-export([init/0,
		version/0,
		transpose/1,
		multiply/2
		]).
-on_load(init/0).

init() ->
    erlang:set_cookie(node(), 'monster'),
	ok.

version() -> 
	rpc({version,{[]}}).

transpose(X)->
	rpc({transpose,{X}}).

multiply(A,B)->
	rpc({multiply,{A,B}}).

rpc({Func,Args}) ->
    {gonum,'gonum@localhost'}!{self(),Func,Args},
    receive
         {ok, Reply} -> Reply
    after
        5000 -> timeout
    end.
