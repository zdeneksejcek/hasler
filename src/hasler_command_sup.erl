-module(hasler_command_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,start_command_fsm/2,register_command/2,lookup_command/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).
-define(COMMANDS, commands).

%% ===================================================================
%% API functions
%% ===================================================================
start_command_fsm(Node, {Name, Args}) ->
    case lookup_command(Name) of
        {_, Module} ->
            supervisor:start_child({?MODULE, Node}, [Module, {Name, Args}]);
        not_found ->
            not_found
    end.

register_command(Name, Module) ->
    ets:insert(?COMMANDS, {Name, Module}),
    io:format("command registered: ~p ! ~p~n",[Module,Name]),
    ok.

lookup_command(Name) ->
    case ets:lookup(?COMMANDS, Name) of
        [] -> not_found;
        [{_,Module}] -> {Name, Module}
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    ok = init_registry(),
    Commands = ?CHILD(hasler_command_fsm, worker),
    
    {ok, {{simple_one_for_one, 10, 10}, [Commands]}}.

%% Commands registry
init_registry() ->
    ets:new(?COMMANDS, [set, public, named_table]),
    ok.

