-module(handler).
-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [{init,1},
     {default_state,0},
     {command, 3},
     {event, 3}];

behaviour_info(_Other) ->
    undefined.