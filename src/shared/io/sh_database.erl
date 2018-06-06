-module(sh_database).

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
      bm_battlemap:type(),
      list(bm_location:type()),
      list(bm_character:type())
   )
   -> list(bm_character:type()).
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
      bm_character:random
      (
         TotalCharacterCount,
         list_to_binary(integer_to_list(MaxPlayerID)),
         bm_battlemap:get_width(Battlemap),
         bm_battlemap:get_height(Battlemap),
         ForbiddenLocations
      ),
   Character =
      case MaxPlayerID of
         0 -> bm_character:set_is_active(true, NewCharacter);
         _ -> NewCharacter
      end,

   generate_random_characters
   (
      MaxPlayerID,
      (PlayerCharacterCount - 1),
      CharactersPerPlayer,
      (TotalCharacterCount + 1),
      Battlemap,
      [bm_character:get_location(Character)|ForbiddenLocations],
      [Character|Result]
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_db () -> 'ok'.
generate_db () ->
   BattlemapWidth = sh_roll:between(16, 64),
   BattlemapHeight = sh_roll:between(16, 64),
   Battlemap = bm_battlemap:random(0, BattlemapWidth, BattlemapHeight),
   Characters = generate_random_characters(1, 8, 8, 0, Battlemap, [], []),
   PlayersAsList = [bm_player:new(<<"0">>), bm_player:new(<<"1">>)],
   Battle = bm_battle:new(<<"0">>, PlayersAsList, Battlemap, Characters),

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
