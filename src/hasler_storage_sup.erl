-module(hasler_storage_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_storage_fsm/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Timeout), {I, {I, start_link, []}, transient, Timeout, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_storage_fsm(Storage_name) ->
    supervisor:start_child({?MODULE, node()}, []).

%% starts supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Storage_children = ?CHILD(hasler_storage_fsm, supervisor, 5000),

    {ok,
        {{simple_one_for_one, 5, 10},
            [Storage_children]}}.