-module(kv2).
-behaviour(ra_machine).

-export([
         %% ra_machine
         init/1,
         apply/4,
         %% client api
         put/3,
         delete/2,
         get/2,
         listen/2,
         %% util
         start/1
         ]).

-type key() :: term().
-type value() :: term().
-type command() :: {put, key(), value()} |
                   {delete, key()} |
                   {get, key()} |
                   {listen, key(), pid()}.

-record(state, {store = #{} :: #{key() => value()},
                listeners = #{} :: #{key() => [pid()]}}).

-opaque state() :: #state{}.
-export_type([state/0]).


-spec init(map()) -> state().
init(_Config) ->
    #state{}.

-spec apply(map(), command(), list(), state()) ->
    {state(), Effects :: list(), Reply :: term()}.
apply(_Meta, {put, Key, Value}, Effects0,
      #state{store = Store, listeners = Listeners} = State) ->
    Effects = case Listeners of
                  #{Key := Pids} ->
                      [{send_msg, Pid, {key_update, Key, Value}} ||
                       Pid <- Pids] ++ Effects0;
                  _ -> Effects0
              end,
    {State#state{store = maps:put(Key, Value, Store)}, Effects, ok};
apply(_Meta, {delete, Key}, Effects0,
      #state{store = Store, listeners = Listeners} = State) ->
    Effects = case Listeners of
                  #{Key := Pids} ->
                      [Pid ! {key_deleted, Key} ||
                       Pid <- Pids] ++ Effects0;
                  _ -> Effects0
              end,
    {State#state{store = maps:remove(Key, Store)}, Effects, ok};
apply(_Meta, {get, Key}, Effects,
      #state{store = Store} = State) ->
    Reply = maps:get(Key, Store, not_found),
    {State, Effects, Reply};
apply(_Meta, {listen, Key, Pid}, Effects,
      #state{listeners = Listeners0} = State) ->
    Listeners = maps:update_with(Key, fun(V) -> [Pid | V] end, [Pid], Listeners0),
    {State#state{listeners = Listeners}, Effects, ok}.


start(Name) ->
    Nodes = nodes(),
    ServerIds = [{Name, N} || N <- Nodes],
    ra:start_cluster(Name, {module, ?MODULE, #{}}, ServerIds).

put(Server, Key, Value) ->
    ra:process_command(Server, {put, Key, Value}).

delete(Server, Key) ->
    ra:process_command(Server, {delete, Key}).

get(Server, Key) ->
    ra:process_command(Server, {get, Key}).

listen(Server, Key) ->
    ra:process_command(Server, {listen, Key, self()}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
