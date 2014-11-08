-module(hasler).

% riak core
-export([execute/1]).

% API
execute(Command) ->
    hasler_command_sup:start_command_fsm(node(), Command).