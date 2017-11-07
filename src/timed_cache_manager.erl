-module(timed_cache_manager).
-export(
   [
      new/2,
      start/1,
      fetch/2
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Manager %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager_core_loop (DB) ->
   receive
      terminate -> ets:delete(DB)
   end.

new_database (DB) ->
   ets:new(
      DB,
      [
         set,
         public,
         {keypos, 1},
         {read_concurrency, true}
      ]
   ).

%%%% Timer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
timer_cleanup (DB, ObjectID) ->
   ets:delete(DB, ObjectID).

timer_core_loop (DB, ObjectID, Timeout) ->
   receive
      ok -> timer_core_loop(DB, ObjectID, Timeout);
      terminate -> ok
   after Timeout ->
      timer_cleanup(DB, ObjectID)
   end.

add_timer (DB, ObjectID, Timeout) ->
   spawn(timed_cache_manager, timer_core_loop, [DB, ObjectID, Timeout]).

add_to_cache (DB, ObjectID) ->
   TimerPID = add_timer(DB, ObjectID, 60000),
   Data = nothing, %% Do the actual NoSQL Fetch here.
   ets:insert(DB, {ObjectID, TimerPID, Data}),
   Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new (Manager, DB) ->
   register(
      Manager,
      spawn(
         timed_cache_manager,
         start,
         [DB]
      )
   ).

start (DB) ->
   new_database(DB),
   manager_core_loop(DB).

fetch (DB, ObjectID) ->
   case ets:lookup(DB, ObjectID) of
      [] ->
         add_to_cache(DB, ObjectID);

      [{_, TimerPID, Data}] ->
         TimerPID ! ok,
         Data
   end.
