-module(demo).
-behaviour(ra_machine).

-export([
         start/0,
         start_cluster/2,

         %% ra_machine
         init/1,
         apply/4
         ]).

-record(state, {}).

-opaque state() :: #state{}.

-export_type([
              state/0
              ]).

start() ->
    application:load(ra),
    Dir = filename:join(element(2, application:get_env(ra, data_dir)), node()),
    application:set_env(ra, data_dir, Dir),
    application:ensure_all_started(ra),
    net_kernel:connect_node(a@snowman),
    net_kernel:connect_node(b@snowman),
    net_kernel:connect_node(c@snowman),
    ok.


init(_) ->
    {0, []}.

apply(_Meta, Cmd, Effects, State) when is_integer(Cmd) ->
    State1 = Cmd + State,
    {State1, Effects, State1}.


start_cluster(Name, Nodes) when is_atom(Name) ->
    Servers = [{Name, Node} || Node <- Nodes],
    ra:start_cluster(Name, {module, ?MODULE, #{}}, Servers).







-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
