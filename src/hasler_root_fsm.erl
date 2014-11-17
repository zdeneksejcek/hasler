-module(hasler_root_fsm).
-behaviour(gen_fsm).

-include("../include/hasler.hrl").

%% API
-export([start_link/4]).

%% gen_fsm callbacks
-export([init/1,
         waiting/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {id, root_module, root_state}).

%%%===================================================================
%%% API
%%%===================================================================

% {ok, Pid} | ignore | {error, Error}
start_link(Id, Root_module, Args, Sender) ->
    Result = gen_fsm:start_link(?MODULE, [Id, Root_module, Args, Sender], []),
%%     io:format("start_link result: ~p~n", [Result]),
    Result.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

% {ok, StateName, State} |
% {ok, StateName, State, Timeout} |
% ignore |
% {stop, StopReason}
init([Id, Root_module, Args, Sender]) ->
    case erlang:apply(Root_module, create, [Args]) of
        {ok, {Event_name, Arg_list}} ->
            Root_state = erlang:apply(Root_module, Event_name, [Arg_list, nil]),
            hasler:reply(Sender, ok),
            {ok, waiting, #state{
                            id = Id,
                            root_module = Root_module,
                            root_state = Root_state}};

        Error ->
            hasler:reply(Sender, Error),
            {stop, normal}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
waiting({Command, Sender}, State) ->
    io:format("before: ~p state: ~p ~n~n",[Command, State#state.root_state]),
    case erlang:apply(State#state.root_module, Command, [nil, State#state.root_state]) of
        {ok, {Event_name, Arg_list}} ->
            Root_state = erlang:apply(State#state.root_module, Event_name, [Arg_list, State#state.root_state]),
            NewState = State#state{root_state = Root_state},
            hasler:reply(Sender, ok),
            io:format("after: ~p state: ~p ~n~n",[Command, NewState#state.root_state]),
            {next_state, waiting, NewState};

        Error ->
            hasler:reply(Sender, Error),
            {stop, normal, State}
    end.























%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
