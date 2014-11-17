-module(hasler_vnode).
-behaviour(riak_core_vnode).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-record(state, {partition, vnode_namespace, count}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    VNode_namespace = list_to_atom("roots_" ++ integer_to_list(Partition)),
    ok = init_ets(VNode_namespace),
    ok = init_storage_process(VNode_namespace),
    {ok, #state { partition=Partition, vnode_namespace = VNode_namespace, count=0 }}.

% handles create command
handle_command({Root_id, {create, Root_module, Args}}, Sender, State) ->
%%     io:format("create root: id:~p ~p~n", [Id, Args]),
    case hasler_root_sup:start_root_fsm(node(), Root_id, Root_module, Args, Sender) of
        {ok, Pid} ->
            insert_root_ets(State#state.vnode_namespace, Root_id, Pid),
            {noreply, State};

        {error, Message} ->
            hasler:reply(Sender, Message),
            io:format("stopped with: ~p~n", [Message]),
            continue
    end;

%% handles commands without parameters
handle_command({Root_id, Command_name}, Sender, State) ->
%%     io:format("create root: id:~p ~p~n", [Id, Args]),
    case get_root_status(Root_id, State#state.vnode_namespace) of
        {running, Pid} ->
            hasler_root_sup:call_root_fsm(Pid, Command_name, Sender),
            {noreply, State};
        _ ->
            continue
    end;

handle_command(Command, _Sender, State) ->
    io:format("unknown root command: ~p~n", [Command]),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% private ETS
init_ets(Table_name) ->
    Table_name = ets:new(Table_name, [named_table, set, protected]),
    ok.

insert_root_ets(Table_name, Root_id, Pid) ->
    ets:insert(Table_name, {Root_id, Pid}).

get_root_status(Root_id, Table_name) ->
    case ets:lookup(Table_name, Root_id) of
        [] -> non_existent;
        [{_, Pid}] ->
            {running, Pid}
    end.

%% storage
init_storage_process(Storage_name) ->
    ok.