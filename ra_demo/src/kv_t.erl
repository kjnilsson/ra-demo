-module(kv_t).
-behaviour(ra_machine).

-export([
         %% ra_machine
         init/1,
         apply/4,
         %% client api
         put/3,
         delete/2,
         get/2,
         %% util
         start/1
         ]).

-type key() :: term().
-type value() :: term().
-type command() :: {put, key(), value()} |
                   {delete, key()} |
                   {get, key()}.

-record(state, {store = #{} :: #{key() => value()}}).
-opaque state() :: #state{}.
-export_type([state/0]).

-spec init(map()) -> state().
init(_Config) ->
    #state{}.

-spec apply(map(), command(), list(), state()) ->
    {state(), Effects :: list(), Reply :: term()}.
apply(_Meta, {put, Key, Value}, Effects,
      #state{store = Store} = State) ->
    {State#state{store = maps:put(Key, Value, Store)},
     Effects, ok};
apply(_Meta, {delete, Key}, Effects,
      #state{store = Store} = State) ->
    {State#state{store = maps:remove(Key, Store)},
     Effects, ok};
apply(_Meta, {get, Key}, Effects,
      #state{store = Store} = State) ->
    Reply = maps:get(Key, Store, not_found),
    {State, Effects, Reply}.


start(Name) when is_atom(Name) ->
    Nodes = nodes(),
    ServerIds = [{Name, N} || N <- Nodes],
    ra:start_cluster(Name, {module, ?MODULE, #{}}, ServerIds).

put(Server, Key, Value) ->
    ra:process_command(Server, {put, Key, Value}).

delete(Server, Key) ->
    ra:process_command(Server, {delete, Key}).

get(Server, Key) ->
    ra:process_command(Server, {get, Key}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
