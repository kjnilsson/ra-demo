-module(kv).
-behaviour(ra_machine).

-export([
         init/1,
         apply/4,

         %% client api
         put/3,
         get/2,
         delete/2,

         %% cluster
         start/1

         ]).

-record(state, {store = #{} :: map()}).

-opaque state() :: #state{}.

-type key() :: term().
-type value() :: term().
-type command() :: {put, key(), value()} |
                   {get, key()} |
                   {delete, key()}.

-export_type([
              state/0
              ]).

init(_Config) ->
    #state{}.

-spec apply(map(), command(), list(), #state{}) ->
    {#state{}, list(), Result :: term()}.
apply(_Meta, {put, Key, Value}, Effects, State) ->
    {State#state{store = maps:put(Key, Value, State#state.store)},
     Effects, ok};
apply(_Meta, {get, Key}, Effects, State) ->
    Result = maps:get(Key, State#state.store, not_found),
    {State, Effects, Result};
apply(_Meta, {delete, Key}, Effects, State) ->
    {State#state{store = maps:remove(Key, State#state.store)},
     Effects, ok}.

%% Client api

put(ServerId, Key, Value) ->
    ra:process_command(ServerId, {put, Key, Value}).

get(ServerId, Key) ->
    ra:process_command(ServerId, {get, Key}).

delete(ServerId, Key) ->
    ra:process_command(ServerId, {delete, Key}).

%% cluster api

start(Name) ->
    Servers = [{Name, Node} || Node <- nodes()],
    ra:start_cluster(Name, {module, ?MODULE, #{}}, Servers).

