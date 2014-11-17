-module(hasler).

% riak core
-export([usecase/1, create_root/1, create_root/2, execute_root/2, root/0, reply/2]).

% API
usecase(Command) ->
    hasler_command_sup:start_command_fsm(node(), Command).

create_root(Root_module) ->
    create_root(Root_module, nil).

create_root(Root_module, Root_args) ->
    Id = uuid:generate(),
%%     io:format("root create requested: ~p with id = ~p~n", [Root_module, Id]),
    com(Id, {create, Root_module, Root_args}),
    {sent, Id}.

execute_root(Id, Root_command) ->
    com(Id, Root_command),
    sent.

reply(Receiver, Message) ->
    gen_fsm:send_event(Receiver, Message),
    ok.

root() ->
    ok.

% private
com(Id, Root_command) ->
    DocIdx = riak_core_util:chash_key({<<"root">>, Id}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, hasler),
    [{IndexNode, _Type}] = PrefList,
    ok = riak_core_vnode_master:command(IndexNode, {Id, Root_command}, self(), hasler_vnode_master).