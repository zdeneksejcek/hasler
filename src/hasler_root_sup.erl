-module(hasler_root_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,start_root_fsm/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_root_fsm(Node, Id, Root_module, Args) ->
    supervisor:start_child({?MODULE, Node}, [Id, Root_module, Args]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("hasler root supervisor started: ~n", []),
    Commands = ?CHILD(hasler_root_fsm, worker),
    
    {ok, {{simple_one_for_one, 10, 10}, [Commands]}}.