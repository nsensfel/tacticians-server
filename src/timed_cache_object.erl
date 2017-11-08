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
      fetch/3,
      invalidate/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_to_cache (DB, ObjectID, Timeout) ->
   {ok, TimerPID} = gen_server:start(?MODULE, [{DB, ObjectID, Timeout}], []),
   Data = nothing, %% Do the actual NoSQL Fetch here.
   ets:insert(DB, {ObjectID, TimerPID, Data}),
   Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% gen_server

init ({DB, ObjectID, Timeout}) ->
   {ok, {DB, ObjectID, Timeout}}.

handle_call (invalidate, _, State) ->
   {stop, normal, State};
handle_call (ping, _, {DB, ObjectID, Timeout}) ->
   {noreply, {DB, ObjectID, Timeout}, Timeout}.

handle_cast (invalidate, State) ->
   {stop, normal, State};
handle_cast (ping, {DB, ObjectID, Timeout}) ->
   {noreply, {DB, ObjectID, Timeout}, Timeout}.

terminate (_, {DB, ObjectID, _Timeout}) ->
   ets:delete(DB, ObjectID).

code_change (_, State, _) ->
   {ok, State}.

format_status (_, [_, State]) ->
   [{data, [{"State", State}]}].

handle_info(timeout, State) ->
   {stop, normal, State};
handle_info(_, {DB, ObjectID, Timeout}) ->
   {noreply, {DB, ObjectID, Timeout}, Timeout}.

%%%% interface
fetch (DB, ObjectID, Timeout) ->
   case ets:lookup(DB, ObjectID) of
      [] -> add_to_cache(DB, ObjectID, Timeout);

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
