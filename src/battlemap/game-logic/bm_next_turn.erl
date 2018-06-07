-module(bm_next_turn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      update_if_needed/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_player_turn_to_next (bm_battle:type())
   -> {bm_battle:type(), sh_db_query:op()}.
set_player_turn_to_next (Battle) ->
   Players = bm_battle:get_players(Battle),
   CurrentPlayerTurn = bm_battle:get_current_player_turn(Battle),

   NextPlayerTurn = bm_player_turn:next(array:size(Players), CurrentPlayerTurn),

   UpdatedBattle = bm_battle:set_current_player_turn(NextPlayerTurn, Battle),

   DBQuery =
      sh_db_query:set_field
      (
         bm_battle:get_current_player_turn_field(),
         NextPlayerTurn
      ),

   {UpdatedBattle, DBQuery}.

-spec reset_next_player_timeline (bm_battle:type())
   -> {bm_battle:type(), bm_player:type(), sh_db_query:op()}.
reset_next_player_timeline (Battle) ->
   NextPlayerTurn = bm_battle:get_current_player_turn(Battle),
   NextPlayerIX = bm_player_turn:get_player_ix(NextPlayerTurn),
   NextPlayer = bm_battle:get_player(NextPlayerIX, Battle),

   UpdatedNextPlayer = bm_player:reset_timeline(NextPlayer),
   UpdatedBattle =
      bm_battle:set_player(NextPlayerIX, UpdatedNextPlayer, Battle),

   DBQuery =
      sh_db_query:update_indexed
      (
         bm_battle:get_players_field(),
         NextPlayerIX,
         [ sh_db_query:set_field(bm_player:get_timeline_field(), []) ]
      ),

   {UpdatedBattle, UpdatedNextPlayer, DBQuery}.


-spec activate_next_players_characters (bm_battle:type(), bm_player:type())
   -> {bm_battle:type(), list(sh_db_query:op())}.
activate_next_players_characters (Battle, NextPlayer) ->
   NextPlayerID = bm_player:get_id(NextPlayer),
   Characters = bm_battle:get_characters(Battle),

   {UpdatedCharacters, ModifiedIXs} =
      sh_array_util:mapiff
      (
         fun (Character) ->
            (bm_character:get_owner_id(Character) == NextPlayerID)
         end,
         fun (Character) ->
            bm_character:set_is_active(true, Character)
         end,
         Characters
      ),

   DBQueries =
      lists:map
      (
         fun (IX) ->
            sh_db_query:update_indexed
            (
               bm_battle:get_characters_field(),
               IX,
               [
                  sh_db_query:set_field
                  (
                     bm_character:get_active_field(),
                     true
                  )
               ]
            )
         end,
         ModifiedIXs
      ),

   UpdatedBattle = bm_battle:set_characters(UpdatedCharacters, Battle),

   {UpdatedBattle, DBQueries}.

-spec update
   (
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
update (Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),

   {S0Battle, DBQuery0} = set_player_turn_to_next(Battle),
   {S1Battle, NextPlayer, DBQuery1} = reset_next_player_timeline(S0Battle),
   {S2Battle, DBQueries} =
      activate_next_players_characters(S1Battle, NextPlayer),

   S0Data = bm_character_turn_data:set_battle(S2Battle, Data),
   S0Update = bm_character_turn_update:set_data(S0Data, Update),

   S1Update =
      lists:foldl
      (
         fun bm_character_turn_update:add_to_db/2,
         S0Update,
         [DBQuery0|[DBQuery1|DBQueries]]
      ),

   S1Update.

-spec requires_update (bm_character_turn_update:type()) -> boolean().
requires_update (Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   Characters = bm_battle:get_characters(Battle),

   sh_array_util:none(fun bm_character:get_is_active/1, Characters).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_if_needed
   (
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
update_if_needed (Update) ->
   case requires_update(Update) of
      true -> update(Update);
      _ -> Update
   end.
