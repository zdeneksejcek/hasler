-module(usecase).
-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [{init,1},
     {default_state,0},
     {command, 4}];

behaviour_info(_Other) ->
    undefined.