-module(timed_caches_manager).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' Exports
-export(
   [
      init/1,
      handle_cast/2,
      handle_call/3,
      terminate/2,
      code_change/3,
      format_status/2,
      handle_info/2
   ]
).

%%%% Actual Interface
-export(
   [
      add_cache/3,
      inherit_cache/3,
      delete_cache/2,
      get_timeout/1
   ]
)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_cache (DB) ->
   ets:delete(DB).

add_cache (DB, none) ->
   io:format("~nTimed Caches Manager added a new cache. ~n"),
   ets:new(
      DB,
      [
         set,
         public,
         named_table,
         {keypos, 2},
         {read_concurrency, true},
         {heir, none}
      ]
   );
add_cache (DB, Heir) ->
   io:format("~nTimed Caches Manager added a new cache. ~n"),
   ets:new(
      DB,
      [
         set,
         public,
         named_table,
         {keypos, 2},
         {read_concurrency, true},
         {heir, Heir, DB}
      ]
   ).

inherit_cache (CacheList, DB, Heir) ->
   case lists:member(DB, CacheList) of
      true ->
         ets:setopts(DB, {heir, Heir, DB}),
         CacheList;

      false ->
         [DB|CacheList]
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' functions
init (CacheList) ->
   io:format("~nStarting Timed Caches Manager..."),
   {ok, CacheList}.

handle_call ({delete, CacheName}, _Caller, State) ->
   {noreply, delete_cache(State, CacheName)};
handle_call ({add, CacheName, Heir}, _Caller, State)->
   {noreply, add_cache(State, CacheName, Heir)};
handle_call ({inherit, CacheName, Heir}, _Caller, State)->
   {noreply, inherit_cache(State, CacheName, Heir)};
handle_call (terminate, _, State) ->
   {stop, normal, State}.

handle_cast ({delete, CacheName}, State) ->
   {noreply, delete_cache(State, CacheName)};
handle_cast ({add, CacheName, Heir}, State)->
   {noreply, add_cache(State, CacheName, Heir)};
handle_cast ({inherit, CacheName, Heir}, State)->
   {noreply, inherit_cache(State, CacheName, Heir)};
handle_cast (terminate, State) ->
   {stop, normal, State}.

terminate (_Reason, []) ->
   ok;
terminate (Reason, [CacheName|OtherCaches]) ->
   delete_cache(CacheName),
   terminate(Reason, OtherCaches).

code_change (_, State, _) ->
   {ok, State}.

format_status (_, [_, State]) ->
   [{data, [{"State", State}]}].

handle_info(_, State) ->
   {noreply, State}.

%%%% Interface Functions
delete_cache (CacheList, DB) ->
   case lists:member(DB, CacheList) of
      true ->
         delete_cache(DB),
         lists:delete(DB, CacheList);
      false ->
         CacheList
   end.

add_cache (CacheList, DB, Heir) ->
   case lists:member(DB, CacheList) of
      true when (Heir =:= none) ->
         CacheList;

      true ->
         ets:setopts(DB, {heir, Heir, DB}),
         CacheList;

      false ->
         add_cache(DB, Heir),
         [DB|CacheList]
   end.

get_timeout(battlemap_db) ->
   60000;
get_timeout(_) ->
   60000.
