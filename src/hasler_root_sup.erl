-module(hasler_root_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,start_root_fsm/5,call_root_fsm/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_root_fsm(Node, Id, Root_module, Root_args, Sender) ->
    supervisor:start_child({?MODULE, Node}, [Id, Root_module, Root_args, Sender]).

call_root_fsm(Pid, Command, Sender) ->
    gen_fsm:send_event(Pid, {Command, Sender}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("hasler root supervisor started: ~n", []),
    Root_commands = ?CHILD(hasler_root_fsm, worker),
    
    {ok, {{simple_one_for_one, 10, 10}, [Root_commands]}}.