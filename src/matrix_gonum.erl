-module(matrix_gonum).
-export([init/0,
		version/0,
		transpose/1,
		multiply/2
		]).
init() ->
	ok.

version() -> 
	rpc({version,[]}).

transpose(_)->
	not_implemented.

multiply(_,_)->
	not_implemented.

rpc({Func,Args}) ->
    {gonum,'gonum@localhost'}!{self(),Func,Args},
    receive
         {ok, Reply} -> Reply
    after
        5000 -> timeout
    end.
