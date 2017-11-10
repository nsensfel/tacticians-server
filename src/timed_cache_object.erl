-module(timed_cache_object).
-behavior(gen_server).

%%%% gen_server exports
-export(
   [
      init/1,
      handle_cast/2,
      handle_call/3, %% No reply will ever be given.
      terminate/2,
      code_change/3,
      format_status/2,
      handle_info/2
   ]
).

%%%% actual interface
-export(
   [
      fetch/2,
      invalidate/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_to_cache (DB, ObjectID) ->
   {ok, TimerPID} = gen_server:start(?MODULE, [{DB, ObjectID}], []),
   {ok, Data} = shim_database:fetch(DB, ObjectID),
   ets:insert(DB, {ObjectID, TimerPID, Data}),
   Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% gen_server

init ({DB, ObjectID}) ->
   {ok, {DB, ObjectID}}.

handle_call (invalidate, _, State) ->
   {stop, normal, State};
handle_call (ping, _, {DB, ObjectID}) ->
   {noreply, {DB, ObjectID}, timed_caches_manager:get_timeout(DB)}.

handle_cast (invalidate, State) ->
   {stop, normal, State};
handle_cast (ping, {DB, ObjectID}) ->
   {noreply, {DB, ObjectID}, timed_caches_manager:get_timeout(DB)}.

terminate (_, {DB, ObjectID}) ->
   ets:delete(DB, ObjectID).

code_change (_, State, _) ->
   {ok, State}.

format_status (_, [_, State]) ->
   [{data, [{"State", State}]}].

handle_info(timeout, State) ->
   {stop, normal, State};
handle_info(_, {DB, ObjectID}) ->
   {noreply, {DB, ObjectID}, timed_caches_manager:get_timeout(DB)}.

%%%% interface
fetch (DB, ObjectID) ->
   case ets:lookup(DB, ObjectID) of
      [] -> add_to_cache(DB, ObjectID);

      [{_, TimerPID, Data}] ->
         gen_server:cast(TimerPID, ping),
         Data
   end.

invalidate (DB, ObjectID) ->
   case ets:lookup(DB, ObjectID) of
      [] -> ok;

      [{_, TimerPID, _}] ->
         gen_server:stop(TimerPID)
   end.
