-module(database_shim).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      generate_db/1,
      fetch/2,
      commit/4
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_db (pid()) -> 'ok'.
create_db (_Heir) ->
   ets:new
   (
      db_shim,
      [
         set,
         public,
         named_table,
         {keypos, 1},
         {read_concurrency, true}
      ]
   ),
   io:format("~ndb_shim ets created.~n"),
   ok.

-spec add_to_db (any(), any()) -> 'ok'.
add_to_db (ID, Val) ->
   io:format("~nadd to db_shim: ~p.~n", [{ID, Val}]),
   ets:insert(db_shim, {ID, Val}),
   ok.

-spec generate_random_characters
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      list(character:type())
   )
   -> list(character:type()).
generate_random_characters
(
   0,
   0,
   _CharactersPerPlayer,
   _TotalCharacterCount,
   Result
) ->
   Result;
generate_random_characters
(
   MaxPlayerID,
   0,
   CharactersPerPlayer,
   TotalCharacterCount,
   Result
) ->
   generate_random_characters
   (
      (MaxPlayerID - 1),
      CharactersPerPlayer,
      CharactersPerPlayer,
      TotalCharacterCount,
      Result
   );
generate_random_characters
(
   MaxPlayerID,
   PlayerCharacterCount,
   CharactersPerPlayer,
   TotalCharacterCount,
   Result
) ->
   NewCharacter =
      character:random
      (
         TotalCharacterCount,
         list_to_binary(integer_to_list(MaxPlayerID))
      ),
   generate_random_characters
   (
      MaxPlayerID,
      (PlayerCharacterCount - 1),
      CharactersPerPlayer,
      (TotalCharacterCount + 1),
      [NewCharacter|Result]
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_db (pid()) -> 'ok'.
generate_db (Heir) ->
   Pid = self(),
   spawn(fun () -> create_db(Heir), Pid ! ok, receive ok -> ok end end),
   receive
      ok -> ok
   end,
   BattlemapWidth = roll:between(16, 64),
   BattlemapHeight = roll:between(16, 64),
   Battlemap = battlemap:random(0, BattlemapWidth, BattlemapHeight),
   Characters = generate_random_characters(1, 8, 8, 0, []),
   PlayersAsList = [player:new(<<"0">>), player:new(<<"1">>)],
   Battle =
      battle:random
      (
         <<"0">>,
         PlayersAsList,
         Battlemap,
         Characters
      ),

   add_to_db({battle_db, <<"0">>}, Battle).

-spec fetch (atom(), any()) -> ({'ok', any()} | 'nothing').
fetch (DB, ObjectID) ->
   io:format("~ndb_shim lookup: ~p.~n", [{DB, ObjectID}]),
   case ets:lookup(db_shim, {DB, ObjectID}) of
      [{_Key, Value}] -> {ok, Value};
      [] -> nothing
   end.

-spec commit (atom(), any(), any(), any()) -> 'ok'.
commit (DB, _Owner, ObjectID, Value) ->
   add_to_db({DB, ObjectID}, Value).
