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
      orddict:orddict(non_neg_integer(), btl_character:type())
   )
   ->
   {
      orddict:orddict(non_neg_integer(), btl_character:type()),
      list(ataxic:basic())
   }.
mark_players_characters_as_defeated (PlayerIX, Characters) ->
   orddict:fold
   (
      fun (IX, Character, {Dict, Updates}) ->
         case (btl_character:get_player_index(Character) == PlayerIX) of
            false -> {Dict, Updates};
            true ->
               {
                  orddict:store
                  (
                     IX,
                     btl_character:set_is_defeated(true, Character),
                     Dict
                  ),
                  [
                     ataxic_sugar:update_orddict_element
                     (
                        IX,
                        ataxic:update_field
                        (
                           btl_character:get_is_defeated_field(),
                           ataxic:constant(true)
                        )
                     )
                  ]
               }
         end
      end,
      {Characters, []},
      Characters
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
   {UpdatedCharacters, AtaxicUpdates} =
      mark_players_characters_as_defeated(PlayerIX, Characters),

   S0Battle = btl_battle:set_characters(UpdatedCharacters, Battle),
   S1Battle =
      btl_battle:set_player
      (
         PlayerIX,
         btl_player:set_is_active
         (
            false,
            btl_battle:get_player(PlayerIX, S0Battle)
         ),
         S0Battle
      ),

   UpdatedData = btl_character_turn_data:set_battle(S1Battle, Data),
   S0Update = btl_character_turn_update:set_data(UpdatedData, Update),

   DBQuery =
      ataxic:sequence
      (
         [
            ataxic:update_field
            (
               btl_battle:get_players_field(),
               ataxic_sugar:update_orddict_element
               (
                  PlayerIX,
                  ataxic:update_field
                  (
                     btl_player:get_is_active_field(),
                     ataxic:constant(false)
                  )
               )
            ),
            ataxic:update_field
            (
               btl_battle:get_characters_field(),
               ataxic:sequence(AtaxicUpdates)
            )
         ]
      ),

   S1Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_player_lost(PlayerIX),
         DBQuery,
         S0Update
      ),

   S1Update.


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
            lists:any
            (
               fun ({IX, Char}) ->
                  (
                     (CharacterPlayerIX == btl_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and btl_character:get_is_alive(Char)
                  )
               end,
               orddict:to_list(Characters)
            ),

         case StillHasAliveChar of
            true -> Update;
            _ -> handle_player_defeat(CharacterPlayerIX, Update)
         end;

      commander -> handle_player_defeat(CharacterPlayerIX, Update);

      target ->
         StillHasAliveChar =
            lists:any
            (
               fun ({IX, Char}) ->
                  (
                     (CharacterPlayerIX == btl_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and btl_character:get_is_alive(Char)
                  )
               end,
               orddict:to_list(Characters)
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
