-module(shr_timed_cache).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' Exports
-export
(
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

%%%% Actual Interface
-export
(
   [
      fetch/3,
      update/4,
      invalidate/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add_to_cache (atom(), any(), any()) -> any().
add_to_cache (DB, Owner, ObjectID) ->
   {ok, TimerPID} = gen_server:start(?MODULE, {DB, {Owner, ObjectID}}, []),
   {ok, Data} = shr_database:fetch(DB, ObjectID, Owner),
   ets:insert(DB, {{Owner, ObjectID}, TimerPID, Data}),
   Data.

-spec add_update_to_cache (atom(), any(), any(), any()) -> 'ok'.
add_update_to_cache (DB, Owner, ObjectID, Data) ->
   {ok, TimerPID} = gen_server:start(?MODULE, {DB, {Owner, ObjectID}}, []),
   ets:insert(DB, {{Owner, ObjectID}, TimerPID, Data}),
   ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' functions
init ({DB, ObjectID}) ->
   io:format("~nCache entry added: ~p.~n", [{DB, ObjectID}]),
   {ok, {DB, ObjectID}, shr_timed_caches_manager:get_timeout()}.

handle_call (invalidate, _, State) ->
   {stop, normal, State};
handle_call (ping, _, State) ->
   {noreply, State, shr_timed_caches_manager:get_timeout()}.

handle_cast (invalidate, State) ->
   {stop, normal, State};
handle_cast (ping, State) ->
   {noreply, State, shr_timed_caches_manager:get_timeout()}.

terminate (_, {DB, ObjectID}) ->
   io:format
   (
      "~nCache entry timed out or was invalidated: ~p.~n",
      [{DB, ObjectID}]
   ),
   ets:delete(DB, ObjectID).

code_change (_, State, _) ->
   {ok, State}.

format_status (_, [_, State]) ->
   [{data, [{"State", State}]}].

handle_info(timeout, State) ->
   {stop, normal, State};
handle_info(_, {DB, ObjectID}) ->
   {noreply, {DB, ObjectID}, shr_timed_caches_manager:get_timeout()}.

%%%% Interface Functions
-spec fetch (atom(), any(), any()) -> any().
fetch (DB, Owner, ObjectID) ->
   io:format("~nfetch from cache: ~p.~n", [{DB, {Owner, ObjectID}}]),
   case ets:lookup(DB, {Owner, ObjectID}) of
      [] -> add_to_cache(DB, Owner, ObjectID);

      [{_, TimerPID, Data}] ->
         gen_server:cast(TimerPID, ping),
         Data
   end.

-spec update (atom(), any(), any(), any()) -> 'ok'.
update (DB, Owner, ObjectID, Data) ->
   io:format("~nUpdating cache: ~p.~n", [{DB, {Owner, ObjectID}}]),
   case ets:lookup(DB, {Owner, ObjectID}) of
      [] -> ok;

      [{_OwnerID, TimerPID, _Data}] ->
         gen_server:stop(TimerPID)
   end,
   add_update_to_cache(DB, Owner, ObjectID, Data).

-spec invalidate (atom(), any(), any()) -> 'ok'.
invalidate (DB, Owner, ObjectID) ->
   case ets:lookup(DB, {Owner, ObjectID}) of
      [] ->
         io:format
         (
            "~nInvalidation request on non-stored entry: ~p.~n",
            [{DB, Owner, ObjectID}]
         ),
         ok;

      [{_, TimerPID, _}] ->
         io:format
         (
            "~nInvalidation request on stored entry: ~p.~n",
            [{DB, Owner, ObjectID}]
         ),
         gen_server:stop(TimerPID),
         ok
   end.
