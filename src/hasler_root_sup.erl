-module(hasler_root_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,start_command_fsm/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_command_fsm(Node, Command) ->
    supervisor:start_child({?MODULE, Node}, [Command]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Commands = ?CHILD(hasler_root_fsm, worker),
    
    {ok, {{simple_one_for_one, 10, 10}, [Commands]}}.

