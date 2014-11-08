-module(hasler_app).
-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
    application:set_env(riak_core, ring_state_dir, "./data"),
    application:set_env(riak_core, ring_creation_size, 16),

    application:start(compiler),
    application:start(syntax_tools),
    application:start(goldrush),
    application:start(lager),
    application:start(sasl),
    application:start(crypto),
    application:start(riak_sysmon),
    application:start(os_mon),
    application:start(basho_stats),
    application:start(eleveldb),
    application:start(pbkdf2),
    application:start(poolboy),
    application:start(riak_core),

    application:start(hasler).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case hasler_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(hasler, [{vnode_module, hasler_vnode}]),
            ok = riak_core_node_watcher:service_up(hasler, self()),
            Services = riak_core_node_watcher:services(),
            io:format("services: ~p", [Services]),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.