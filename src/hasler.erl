-module(hasler).

% riak core
-export([bench/1, execute/1, create_root/2, root/0]).

% API

bench(0) ->
    ok;
bench(N) ->
    hasler:execute({create_sales_order, something}),
    bench(N-1).

execute(Command) ->
    hasler_command_sup:start_command_fsm(node(), Command).

create_root(Root_module, Args) ->
    Id = uuid:generate(),
    io:format("root create requested: ~p with id = ~p~n", [Root_module, Id]),
    com(Id, {create, Root_module, Args}),
    {sent, Id}.

root() ->
    ok.

% private
com(Id, Command) ->
    DocIdx = riak_core_util:chash_key({<<"root">>, Id}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, hasler),
    [{IndexNode, _Type}] = PrefList,
    Result = riak_core_vnode_master:command(IndexNode, {Id, Command}, self(), hasler_vnode_master),
    ok.