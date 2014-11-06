-module(hasler).
-export([start/0,register_uc/2,test/1]).
-export([execute/3,execute/4,usecases_init/0, usecases_loop/0, usecase_loop/1,roots_init/0, roots_loop/0, root_loop/0]).

% riak core
-export([ping/0,com/1,acom/1,bench/1]).

-define(USECASES, usecases).
-define(ROOTS, roots).

test(0) ->
    ok;
test(N) ->
    hasler:execute(uc, ok, 42),
    test(N-1).

% exported functions
start() ->
    start_usecases(),
    start_roots(),
    hasler:register_uc(example_usecase,ok),
    hasler:register_uc(example_usecase,command),
    hasler:register_uc(example_usecase,finished),
    ok.

register_uc(Module, Command) ->
    ?USECASES ! {register_uc, Module, Command},
    ok.

execute(uc, Command, Args) ->
    Ref = make_ref(),
    From = self(),
    ?USECASES ! {execute, Command, Args, From, Ref},
    {ok, Ref}.

execute(root, Id, Command, Args) ->
    Ref = make_ref(),
    From = self(),
    ?ROOTS ! {execute, Id, Command, Args, From, Ref},
    {ok, Ref}.

% usecases
start_usecases() ->
    Pid = spawn(hasler, usecases_init, []),
    register(?USECASES, Pid),
    ok.

usecases_init() ->
    ets:new(?USECASES, [set, named_table]),
    usecases_loop().

usecases_loop() ->
    receive
        {register_uc, Uc_module, Command} ->
            ets:insert(?USECASES, {Command, Uc_module}),
            io:format("~p:~p registered~n", [Uc_module,Command]),
            hasler:usecases_loop();

        {execute, Command, Args, From, Ref} ->
            case ets:lookup(?USECASES, Command) of
                [] ->
                    From ! {unknown_command, Command, Ref},
                    hasler:usecases_loop();

                [{_, Uc_module}|_] ->
                    % io:format("~p", [Uc_module]),
                    spawn_usecase(Uc_module, Command, Args, Ref),
                    hasler:usecases_loop()
            end;
        Other ->
            io:format("~p garbage received", [Other]),
            hasler:usecases_loop()
    end.

spawn_usecase(Uc_module, Command, Args, Ref) ->
    Pid = spawn(hasler, usecase_loop, [Uc_module]),
    Pid ! {Command, Args, Ref},
    ok.

% command(hello, Args, Ref, _State) ->
% uc worker
usecase_loop(Uc_module) ->
    receive
        {Command, Args, Ref} ->
            case Uc_module:command(Command,Args,Ref,nil) of
                {ok, _State} ->
                    hasler:usecase_loop(Uc_module);

                {command, Produced_commands, _State} ->
                    hasler:usecase_loop(Uc_module);

                {finished, _State} ->
                    exit(normal)
            end
    end.

% root
start_roots() ->
    Pid = spawn(hasler, roots_init, []),
    register(roots, Pid),
    ok.

roots_init() ->
    ets:new(?ROOTS, [set, named_table]),
    roots_loop().

roots_loop() ->
    receive
        {execute, _Id, create, Args, From, Ref} ->
            Pid = spawn(hasler, root_loop, []),
            Pid ! {create, Args, From, Ref};

        {execute, Id, Command, Args, From, Ref} ->
            case ets:lookup(?ROOTS, Id) of
                [] ->
                    From ! {root_not_found, Command, Ref},
                    hasler:roots_loop();

                [{_, Pid}|_] ->
                    io:format("Root found~p", [Pid]),
                    Pid ! {Command, Args, From, Ref},
                    hasler:roots_loop()
            end;
        Other ->
            io:format("~p garbage received", [Other]),
            hasler:roots_loop()
    end.

root_loop() ->
    receive
        ok -> ok
    end.


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

