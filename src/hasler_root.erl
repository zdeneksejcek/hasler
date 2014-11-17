-module(hasler_root).
-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [{create, 1}];

behaviour_info(_Other) ->
    undefined.