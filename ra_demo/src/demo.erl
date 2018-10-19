-module(demo).

-export([
         start/0,
         start_cluster/2

         ]).

-record(state, {}).

-opaque state() :: #state{}.

-export_type([
              state/0
              ]).

connect_nodes() ->
    [net_kernel:connect_node(N)
     || N <- [x@snowman, a@snowman, b@snowman, c@snowman]].

start() ->
    ra:start(),
    connect_nodes(),
    ok.



start_cluster(Name, Nodes) when is_atom(Name) ->
    Servers = [{Name, Node} || Node <- Nodes],
    ra:start_cluster(Name, {module, ?MODULE, #{}}, Servers).







-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
