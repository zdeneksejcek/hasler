-module(example_command).
-behaviour(hasler_command).

-export([init/1,default_state/0,command/4]).

init(State) ->
    State.

default_state() ->
    nil.

command(ok, Args, Ref, _State) ->
    io:format("example ok command received = ~p (ref: ~p)~n", [Args, Ref]),
    {ok, _State};

command(command, Args, Ref, _State) ->
    io:format("example command command received = ~p (ref: ~p)~n", [Args, Ref]),
    {command, nil, _State};

command(finished, Args, Ref, _State) ->
    io:format("example finished command received = ~p (ref: ~p)~n", [Args, Ref]),
    {finished, _State}.