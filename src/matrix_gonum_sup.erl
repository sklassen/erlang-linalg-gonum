-module(matrix_gonum_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
	Procs = [
			{gonum,{gonum,start,[gonum,[erlang:get_cookie()]]},permanent,8000,worker,dynamic}
			],
	{ok, {{one_for_one, 10, 10}, Procs}}.

