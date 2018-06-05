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
      generate_db/0,
      fetch/2,
      commit/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_db_node () ->
   list_to_atom("db_node@" ++ net_adm:localhost()).

-spec generate_random_characters
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      battlemap:type(),
      list(location:type()),
      list(character:type())
   )
   -> list(character:type()).
generate_random_characters
(
   0,
   0,
   _CharactersPerPlayer,
   _TotalCharacterCount,
   _Battlemap,
   _ForbiddenLocations,
   Result
) ->
   Result;
generate_random_characters
(
   MaxPlayerID,
   0,
   CharactersPerPlayer,
   TotalCharacterCount,
   Battlemap,
   ForbiddenLocations,
   Result
) ->
   generate_random_characters
   (
      (MaxPlayerID - 1),
      CharactersPerPlayer,
      CharactersPerPlayer,
      TotalCharacterCount,
      Battlemap,
      ForbiddenLocations,
      Result
   );
generate_random_characters
(
   MaxPlayerID,
   PlayerCharacterCount,
   CharactersPerPlayer,
   TotalCharacterCount,
   Battlemap,
   ForbiddenLocations,
   Result
) ->
   NewCharacter =
      character:random
      (
         TotalCharacterCount,
         list_to_binary(integer_to_list(MaxPlayerID)),
         battlemap:get_width(Battlemap),
         battlemap:get_height(Battlemap),
         ForbiddenLocations
      ),
   Character =
      case MaxPlayerID of
         0 -> character:set_is_active(true, NewCharacter);
         _ -> NewCharacter
      end,

   generate_random_characters
   (
      MaxPlayerID,
      (PlayerCharacterCount - 1),
      CharactersPerPlayer,
      (TotalCharacterCount + 1),
      Battlemap,
      [character:get_location(Character)|ForbiddenLocations],
      [Character|Result]
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_db () -> 'ok'.
generate_db () ->
   BattlemapWidth = roll:between(16, 64),
   BattlemapHeight = roll:between(16, 64),
   Battlemap = battlemap:random(0, BattlemapWidth, BattlemapHeight),
   Characters = generate_random_characters(1, 8, 8, 0, Battlemap, [], []),
   PlayersAsList = [player:new(<<"0">>), player:new(<<"1">>)],
   Battle = battle:new(<<"0">>, PlayersAsList, Battlemap, Characters),

   {atomic, ok} =
      rpc:call
      (
         get_db_node(),
         storage_access,
         insert,
         [battle_db, <<"0">>, any, Battle]
      ),

   ok.

-spec fetch (atom(), any()) -> ({'ok', any()} | 'not_found').
fetch (DB, ObjectID) ->
   {atomic, Reply} =
      rpc:call(get_db_node(), storage_access, read, [DB, ObjectID]),
   io:format("~ndb_shim:fetch(~p) -> ~p.~n", [{DB, ObjectID}, Reply]),
   Reply.

-spec commit (db_query:type()) -> 'ok'.
commit (Query) ->
   {atomic, ok} = rpc:call(get_db_node(), storage_access, query, [Query]),
   io:format("~ndb_shim:commit(~p) -> ok.~n", [Query]),
   ok.
