-module(hasler).

% riak core
-export([ping/0,com/1,acom/1,bench/1]).
-export([execute/1]).

% API
execute(Command) ->
    hasler_command_sup:start_command_fsm(node(), Command).

% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, hasler),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, hasler_vnode_master).

com(Command) ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, hasler),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_command(IndexNode, Command, hasler_vnode_master).

acom(Command) ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, hasler),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_command(IndexNode, Command, hasler_vnode_master).

bench(Size) ->
    Start =now(),
    test2(Size),
    Finish=now(),
    io:format("Count is ~p~n",[Size]),
    io:format("Test took ~p seconds~n",[elapsedTime(Start,Finish)]),
    io:format("Throughput=~p per sec~n",[throughput(Size,Start,Finish)]).

test2(Size) ->
    foreach(fun (_X)-> ping() end,lists:seq(1,Size)),
    ok.    

elapsedTime(Start,Finish) -> 
    (toMicroSeconds(Finish) - toMicroSeconds(Start)) /1000000.

toMicroSeconds({MegaSeconds,Seconds,MicroSeconds}) -> 
    (MegaSeconds+Seconds) * 1000000 + MicroSeconds.

throughput(Size,Start,Finish) -> Size / elapsedTime(Start,Finish).


foreach(Fun,List) -> 
    Splits = split(8,List),
    Pids = executeA(Splits,Fun,self()),
    waitCompletion(Pids).

split(1,List) -> [List];
split(Pieces,List) -> 
    Length = length(List),
    N = round(Length/Pieces),
    {List1,Remainder} = lists:split(N,List),
    [List1|split(Pieces-1,Remainder)].

executeA([],_Fun,_EndPid) -> [];
executeA([L|Lists],Fun,EndPid) -> 
    Pid = spawn( fun()->runFun(Fun,L,EndPid) end ),
    [Pid|executeA(Lists,Fun,EndPid)].

runFun(Fun,List,EndPid) -> lists:foreach(Fun,List),
    EndPid ! finished.

waitCompletion(Pids) -> 
    Times = lists:seq(1,length(Pids)),
    lists:foreach(fun (_X) -> 
            receive finished -> ok 
            end
        end, Times).

