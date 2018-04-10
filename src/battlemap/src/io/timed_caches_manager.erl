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
      start/0,
      new_cache/3,
      delete_cache/2,
      get_timeout/0
   ]
)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_cache (DB) ->
   ets:delete(DB).

add_cache (DB, none) ->
   io:format("~nTimed Caches Manager added a new cache. ~n"),
   ets:new(
      DB,
      [
         set,
         public,
         named_table,
         {keypos, 1},
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
         {keypos, 1},
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

remove_cache (CacheList, DB) ->
   case lists:member(DB, CacheList) of
      true ->
         remove_cache(DB),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' functions
init (CacheList) ->
   io:format("~nStarting Timed Caches Manager..."),
   {ok, CacheList}.

handle_call ({remove, CacheName}, _Caller, State) ->
   {noreply, remove_cache(State, CacheName)};
handle_call ({add, CacheName, Heir}, _Caller, State)->
   {noreply, add_cache(State, CacheName, Heir)};
handle_call ({inherit, CacheName, Heir}, _Caller, State)->
   {noreply, inherit_cache(State, CacheName, Heir)};
handle_call (terminate, _, State) ->
   {stop, normal, State}.

handle_cast ({remove, CacheName}, State) ->
   {noreply, remove_cache(State, CacheName)};
handle_cast ({add, CacheName, Heir}, State)->
   {noreply, add_cache(State, CacheName, Heir)};
handle_cast ({inherit, CacheName, Heir}, State)->
   {noreply, inherit_cache(State, CacheName, Heir)};
handle_cast (terminate, State) ->
   {stop, normal, State}.

terminate (_Reason, []) ->
   ok;
terminate (Reason, [CacheName|OtherCaches]) ->
   remove_cache(CacheName),
   terminate(Reason, OtherCaches).

code_change (_, State, _) ->
   {ok, State}.

format_status (_, [_, State]) ->
   [{data, [{"State", State}]}].

handle_info(_, State) ->
   {noreply, State}.

%%%% Interface Functions
start () ->
   gen_server:start(timed_caches_manager, [], []).

new_cache (ManagerPid, DB, Heir) ->
   gen_server:cast(ManagerPid, {add, DB, Heir}).

delete_cache (ManagerPid, DB) ->
   gen_server:cast(ManagerPid, {remove, DB}).

get_timeout () ->
   120000. % 2min.
