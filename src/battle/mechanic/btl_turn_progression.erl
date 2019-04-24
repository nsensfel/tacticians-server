-module(btl_turn_progression).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_player_turn_to_next (btl_battle:type())
   -> {btl_battle:type(), ataxic:basic()}.
set_player_turn_to_next (Battle) ->
   Players = btl_battle:get_players(Battle),
   CurrentPlayerTurn = btl_battle:get_current_player_turn(Battle),

   NextPlayerTurn = btl_player_turn:next(Players, CurrentPlayerTurn),

   UpdatedBattle = btl_battle:set_current_player_turn(NextPlayerTurn, Battle),

   DBQuery =
      ataxic:update_field
      (
         btl_battle:get_current_player_turn_field(),
         ataxic:constant(NextPlayerTurn)
      ),

   {UpdatedBattle, DBQuery}.

-spec reset_next_player_timeline (btl_battle:type())
   -> {btl_battle:type(), btl_player:type(), ataxic:basic()}.
reset_next_player_timeline (Battle) ->
   NextPlayerTurn = btl_battle:get_current_player_turn(Battle),
   NextPlayerIX = btl_player_turn:get_player_ix(NextPlayerTurn),
   NextPlayer = btl_battle:get_player(NextPlayerIX, Battle),

   UpdatedNextPlayer = btl_player:reset_timeline(NextPlayer),
   UpdatedBattle =
      btl_battle:set_player(NextPlayerIX, UpdatedNextPlayer, Battle),

   DBQuery =
      ataxic:update_field
      (
         btl_battle:get_players_field(),
         ataxic_sugar:update_orddict_element
         (
            NextPlayerIX,
            ataxic:update_field
            (
               btl_player:get_timeline_field(),
               ataxic:constant([])
            )
         )
      ),

   {UpdatedBattle, UpdatedNextPlayer, DBQuery}.


-spec activate_next_players_characters
   (
      btl_battle:type(),
      btl_player:type()
   )
   -> {btl_battle:type(), ataxic:basic()}.
activate_next_players_characters (Battle, NextPlayer) ->
   NextPlayerIX = btl_player:get_index(NextPlayer),
   AllCharacters = btl_battle:get_characters(Battle),

   {ResultingBattle, BattleAtaxicUpdates} =
      orddict:fold
      (
         fun (IX, Character, {CurrentBattle, CurrentBattleUpdates}) ->
            case (btl_character:get_player_index(Character) == NextPlayerIX) of
               true ->
                  {UpdatedCharacter, CharacterAtaxicUpdate} =
                     btl_character:ataxia_set_is_active(true, Character),

                  {UpdatedBattle, BattleAtaxicUpdate} =
                     btl_battle:ataxia_set_character
                     (
                        IX,
                        UpdatedCharacter,
                        CharacterAtaxicUpdate
                     ),

                  {UpdatedBattle, [BattleAtaxicUpdate|CurrentBattleUpdates]};

               false -> {CurrentBattle, CurrentBattleUpdates}
            end
         end,
         {Battle, []},
         AllCharacters
      ),

   {ResultingBattle, ataxic:optimize(ataxic:sequence(BattleAtaxicUpdates))}.

-spec activate_next_player
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
activate_next_player (Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),

   {S0Battle, DBQuery0} = prepare_player_turn_for_next_player(Battle),
   {S1Battle, NextPlayer, DBQuery1} = reset_next_player_timeline(S0Battle),
   {S2Battle, DBQuery2} =
      activate_next_players_characters(S1Battle, NextPlayer),

   S0Data = btl_character_turn_data:set_battle(S2Battle, Data),

   S0Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_player_turn_started
         (
            btl_player:get_index(NextPlayer)
         ),
         DBQuery0,
         Update
      ),

   S1Update = btl_character_turn_update:set_data(S0Data, S0Update),

   S2Update =
      lists:foldl
      (
         fun btl_character_turn_update:add_to_db/2,
         S1Update,
         [DBQuery1,DBQuery2]
      ),

   S2Update.

-spec has_active_characters_remaining
   (
      btl_character_turn_update:type()
   )
   -> boolean().
has_active_characters_remaining (Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   Characters = btl_battle:get_characters(Battle),

   lists:any
   (
      fun ({_IX, Char}) -> btl_character:get_is_active(Char) end,
      orddict:to_list(Characters)
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle (Update) ->
   case has_active_characters_remaining(Update) of
      false -> activate_next_player(Update);
      _ -> Update
   end.
