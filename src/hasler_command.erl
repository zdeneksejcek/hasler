-module(hasler_command).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start,1}];

behaviour_info(_Other) ->
    undefined.

