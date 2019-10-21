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
-spec prepare_player_turn_for_next_player
   (
      btl_battle:type()
   )
   -> {non_neg_integer(), btl_battle:type(), ataxic:basic()}.
prepare_player_turn_for_next_player (Battle) ->
   Players = btl_battle:get_players(Battle),
   CurrentPlayerTurn = btl_battle:get_current_player_turn(Battle),

   {UpdatedPlayerTurn, PlayerTurnAtaxiaUpdate} =
      btl_player_turn:ataxia_next(Players, CurrentPlayerTurn),

   {UpdatedBattle, BattleAtaxiaUpdate} =
      btl_battle:ataxia_set_current_player_turn
      (
         UpdatedPlayerTurn,
         PlayerTurnAtaxiaUpdate,
         Battle
      ),

   {
      btl_player_turn:get_player_ix(UpdatedPlayerTurn),
      UpdatedBattle,
      BattleAtaxiaUpdate
   }.

-spec reset_next_player_timeline
   (
      non_neg_integer(),
      btl_battle:type()
   )
   -> {btl_battle:type(), ataxic:basic()}.
reset_next_player_timeline (NextPlayerIX, Battle) ->
   NextPlayer = btl_battle:get_player(NextPlayerIX, Battle),

   {UpdatedNextPlayer, PlayerAtaxiaUpdate} =
      btl_player:ataxia_reset_timeline(NextPlayer),

   {UpdatedBattle, BattleAtaxiaUpdate} =
      btl_battle:ataxia_set_player
      (
         NextPlayerIX,
         UpdatedNextPlayer,
         PlayerAtaxiaUpdate,
         Battle
      ),

   {UpdatedBattle, BattleAtaxiaUpdate}.


-spec activate_next_players_characters
   (
      non_neg_integer(),
      btl_battle:type()
   )
   -> {btl_battle:type(), ataxic:basic()}.
activate_next_players_characters (NextPlayerIX, Battle) ->
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
                        CharacterAtaxicUpdate,
                        CurrentBattle
                     ),

                  {UpdatedBattle, [BattleAtaxicUpdate|CurrentBattleUpdates]};

               false -> {CurrentBattle, CurrentBattleUpdates}
            end
         end,
         {Battle, []},
         AllCharacters
      ),

   {ResultingBattle, ataxic:sequence(BattleAtaxicUpdates)}.

-spec activate_next_player
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
activate_next_player (S0Update) ->
   Battle = btl_character_turn_update:get_battle(S0Update),

   {NextPlayerIX, S0Battle, BattleAtaxiaUpdate0} =
      prepare_player_turn_for_next_player(Battle),

   {S1Battle, BattleAtaxiaUpdate1} =
      reset_next_player_timeline(NextPlayerIX, S0Battle),

   {S2Battle, BattleAtaxiaUpdate2} =
      activate_next_players_characters(NextPlayerIX, S1Battle),

   S1Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S2Battle,
         ataxic:sequence
         (
            [
               BattleAtaxiaUpdate0,
               BattleAtaxiaUpdate1,
               BattleAtaxiaUpdate2
            ]
         ),
         S0Update
      ),

   S2Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_player_turn_started(NextPlayerIX),
         S1Update
      ),

   S2Update.

-spec has_active_characters_remaining
   (
      btl_character_turn_update:type()
   )
   -> boolean().
has_active_characters_remaining (Update) ->
   Battle = btl_character_turn_update:get_battle(Update),
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
      true -> Update
   end.
