-module(bm_victory).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle_character_lost_health/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec mark_players_characters_as_defeated
   (
      non_neg_integer(),
      array:array(bm_character:type())
   ) -> {array:array(bm_character:type()), list(non_neg_integer())}.
mark_players_characters_as_defeated (PlayerIX, Characters) ->
   sh_array_util:mapiff
   (
      fun (Character) ->
         (bm_character:get_player_index(Character) == PlayerIX)
      end,
      fun (Character) ->
         bm_character:set_is_defeated(true, Character)
      end,
      Characters
   ).

-spec add_db_query_to_mark_character_as_defeated
   (
      non_neg_integer(),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
add_db_query_to_mark_character_as_defeated (IX, Update) ->
   bm_character_turn_update:add_to_db
   (
      sh_db_query:update_indexed
      (
         bm_battle:get_characters_field(),
         IX,
         [
            sh_db_query:set_field
            (
               bm_character:get_is_defeated_field(),
               true
            )
         ]
      ),
      Update
   ).

-spec handle_player_defeat
   (
      non_neg_integer(),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
handle_player_defeat (PlayerIX, Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   Characters = bm_battle:get_characters(Battle),

   %% FIXME: The controlled character might slip through.
   {UpdatedCharacters, ModifiedIXs} =
      mark_players_characters_as_defeated(PlayerIX, Characters),

   S1Update =
      lists:foldl
      (
         fun add_db_query_to_mark_character_as_defeated/2,
         Update,
         ModifiedIXs
      ),

   %% TODO: Battle.player[PlayerIX].is_active <- false

   UpdatedBattle = bm_battle:set_characters(UpdatedCharacters, Battle),
   UpdatedData = bm_character_turn_data:set_battle(UpdatedBattle, Data),
   S2Update = bm_character_turn_update:set_data(UpdatedData, S1Update),

   DBQuery =
      sh_db_query:update_indexed
      (
         bm_battle:get_players_field(),
         PlayerIX,
         [
            sh_db_query:set_field
            (
               bm_player:get_is_active_field(),
               false
            )
         ]
      ),

   S3Update =
      bm_character_turn_update:add_to_timeline
      (
         bm_turn_result:new_player_lost(PlayerIX),
         DBQuery,
         S2Update
      ),

   S3Update.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_character_lost_health
   (
      non_neg_integer(),
      integer(),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
handle_character_lost_health (_, Health, Update) when (Health > 0) -> Update;
handle_character_lost_health (CharIX, _Health, Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   Character = bm_battle:get_character(CharIX, Battle),
   Characters = bm_battle:get_characters(Battle),
   CharacterPlayerIX = bm_character:get_player_index(Character),

   case bm_character:get_rank(Character) of
      optional ->
         %% Let's not assume there is a commander
         StillHasAliveChar =
            sh_array_util:any_indexed
            (
               fun (IX, Char) ->
                  (
                     (CharacterPlayerIX == bm_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and bm_character:get_is_alive(Char)
                  )
               end,
               %% FIXME: Potential issue if it's the controlled player and Data
               %% is dirty.
               Characters
            ),

         case StillHasAliveChar of
            true -> Update;
            _ -> handle_player_defeat(CharacterPlayerIX, Update)
         end;

      commander -> handle_player_defeat(CharacterPlayerIX, Update);

      target ->
         StillHasAliveChar =
            sh_array_util:any_indexed
            (
               fun (IX, Char) ->
                  (
                     (CharacterPlayerIX == bm_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and bm_character:get_is_alive(Char)
                     and (bm_character:get_rank(Char) == target)
                  )
               end,
               %% FIXME: Potential issue if it's the controlled player and Data
               %% is dirty.
               Characters
            ),

         case StillHasAliveChar of
            true -> Update;
            _ -> handle_player_defeat(CharacterPlayerIX, Update)
         end
   end.
