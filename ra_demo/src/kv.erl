-module(kv).
-behaviour(ra_machine).

-export([
         %% ra_machine implementation
         init/1,
         apply/4,

         %% Client api
         put/3,
         get/2,

         %% Cluster management API
         start/2,
         replace_member/3

         ]).


init(_Config) -> #{}.

apply(_Meta, {put, Key, Value}, Effects, State) ->
    {maps:put(Key, Value, State), Effects, inserted};
apply(_Meta, {get, Key}, Effects, State) ->
    Result = maps:get(Key, State, key_not_found),
    {State, Effects, Result}.


%% Client api

put(ServerId, Key, Value) ->
    ra:process_command(ServerId, {put, Key, Value}).

get(ServerId, Key) ->
    ra:process_command(ServerId, {get, Key}).

%% cluster api

start(Name, Nodes) when is_atom(Name) ->
    %% Create some server ids
    %% Each server has the same name on each erlang node
    ServerIds = [{Name, N} || N <- Nodes],
    %% machine config tells ra which module implements the ra_behaviour
    MachineConf = {module, ?MODULE, #{}},
    ra:start_cluster(Name, MachineConf, ServerIds).

replace_member(ServerId, ServerIdToRemove, ServerIdToAdd) ->
    %% get the current members of the cluster
    {ok, Members, Leader} = ra:members(ServerId),
    %% start a new server
    %% by default a new server will just wait to receive a message
    ok = ra:start_server(element(1, ServerId), ServerIdToAdd,
                        {module, ?MODULE, #{}}, Members),
    %% add a new member
    {ok, _, _} = ra:add_member(ServerId, ServerIdToAdd),
    %% remove the retired member
    {ok, _, _} = ra:remove_member(Leader, ServerIdToRemove),
    %% shutdown and delete the retired member's data
    ok = ra:delete_server(ServerIdToRemove),
    ok.
