-module(btl_victory).
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
      array:array(btl_character:type())
   ) -> {array:array(btl_character:type()), list(non_neg_integer())}.
mark_players_characters_as_defeated (PlayerIX, Characters) ->
   shr_array_util:mapiff
   (
      fun (Character) ->
         (btl_character:get_player_index(Character) == PlayerIX)
      end,
      fun (Character) ->
         btl_character:set_is_defeated(true, Character)
      end,
      Characters
   ).

-spec add_db_query_to_mark_character_as_defeated
   (
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
add_db_query_to_mark_character_as_defeated (IX, Update) ->
   btl_character_turn_update:add_to_db
   (
      shr_db_query:update_indexed
      (
         btl_battle:get_characters_field(),
         IX,
         [
            shr_db_query:set_field
            (
               btl_character:get_is_defeated_field(),
               true
            )
         ]
      ),
      Update
   ).

-spec handle_player_defeat
   (
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_player_defeat (PlayerIX, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   Characters = btl_battle:get_characters(Battle),

   %% FIXME [FUNCTION: battle][MEDIUM]: The controlled character might slip
   %% through.
   {UpdatedCharacters, ModifiedIXs} =
      mark_players_characters_as_defeated(PlayerIX, Characters),

   S1Update =
      lists:foldl
      (
         fun add_db_query_to_mark_character_as_defeated/2,
         Update,
         ModifiedIXs
      ),

   %% TODO [FUNCTION: battle][MEDIUM]: Battle.player[PlayerIX].is_active <-
   %% false

   UpdatedBattle = btl_battle:set_characters(UpdatedCharacters, Battle),
   UpdatedData = btl_character_turn_data:set_battle(UpdatedBattle, Data),
   S2Update = btl_character_turn_update:set_data(UpdatedData, S1Update),

   DBQuery =
      shr_db_query:update_indexed
      (
         btl_battle:get_players_field(),
         PlayerIX,
         [
            shr_db_query:set_field
            (
               btl_player:get_is_active_field(),
               false
            )
         ]
      ),

   S3Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_player_lost(PlayerIX),
         DBQuery,
         S2Update
      ),

   S3Update.


-spec actually_handle_character_lost_health
   (
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
actually_handle_character_lost_health (CharIX, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   Character = btl_battle:get_character(CharIX, Battle),
   Characters = btl_battle:get_characters(Battle),
   CharacterPlayerIX = btl_character:get_player_index(Character),

   case btl_character:get_rank(Character) of
      optional ->
         %% Let's not assume there is a commander
         StillHasAliveChar =
            shr_array_util:any_indexed
            (
               fun (IX, Char) ->
                  (
                     (CharacterPlayerIX == btl_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and btl_character:get_is_alive(Char)
                  )
               end,
               Characters
            ),

         case StillHasAliveChar of
            true -> Update;
            _ -> handle_player_defeat(CharacterPlayerIX, Update)
         end;

      commander -> handle_player_defeat(CharacterPlayerIX, Update);

      target ->
         StillHasAliveChar =
            shr_array_util:any_indexed
            (
               fun (IX, Char) ->
                  (
                     (CharacterPlayerIX == btl_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and btl_character:get_is_alive(Char)
                     and (btl_character:get_rank(Char) == target)
                  )
               end,
               Characters
            ),

         case StillHasAliveChar of
            true -> Update;
            _ -> handle_player_defeat(CharacterPlayerIX, Update)
         end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_character_lost_health
   (
      non_neg_integer(),
      integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_character_lost_health (_, Health, Update) when (Health > 0) -> Update;
handle_character_lost_health (CharIX, _Health, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   S1Data = btl_character_turn_data:clean_battle(Data),
   S1Update = btl_character_turn_update:set_data(S1Data, Update),

   S2Update = actually_handle_character_lost_health(CharIX, S1Update),

   S2Data = btl_character_turn_update:get_data(S2Update),
   S3Data = btl_character_turn_data:refreshr_character(S2Data),
   S3Update = btl_character_turn_update:set_data(S3Data, S2Update),

   S3Update.
