-module(example_root).
-behaviour(root).
-export([command/3,default_state/0,event/3,init/1]).

init(State) ->
    {ok, State}.

default_state() ->
     ok.
%    #{guid=>nil, code => nil}.

command(_Type, _Args, _State) ->
    ok.

event(_Type, _Args, _State) ->
    ok.
