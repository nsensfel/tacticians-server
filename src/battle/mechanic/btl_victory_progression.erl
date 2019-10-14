-module(btl_victory_progression).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle_character_loss/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec mark_characters_of_player_as_defeated
   (
      non_neg_integer(),
      btl_battle:type()
   )
   ->
   {
      btl_battle:type(),
      ataxic:basic()
   }.
mark_characters_of_player_as_defeated (PlayerIX, Battle) ->
   AllCharacters = btl_battle:get_characters(Battle),

   {ResultingBattle, BattleAtaxiaUpdates} =
      orddict:fold
      (
         fun (IX, Character, {CurrentBattle, CurrentBattleAtaxiaUpdates}) ->
            case (btl_character:get_player_index(Character) == PlayerIX) of
               false -> {CurrentBattle, CurrentBattleAtaxiaUpdates};
               true ->
                  {UpdatedCharacter, CharacterAtaxiaUpdate} =
                     btl_character:ataxia_set_is_defeated(true, Character),

                  {UpdatedBattle, NewBattleAtaxiaUpdate} =
                     btl_battle:ataxia_set_character
                     (
                        IX,
                        UpdatedCharacter,
                        CharacterAtaxiaUpdate,
                        Battle
                     ),

                  {
                     UpdatedBattle,
                     [NewBattleAtaxiaUpdate|CurrentBattleAtaxiaUpdates]
                  }
            end
         end,
         {Battle, []},
         AllCharacters
      ),

   {ResultingBattle, ataxic:optimize(ataxic:sequence(BattleAtaxiaUpdates))}.

-spec mark_player_as_inactive
   (
      non_neg_integer(),
      btl_battle:type()
   )
   -> {btl_battle:type(), ataxic:basic()}.
mark_player_as_inactive (PlayerIX, Battle) ->
   Player = btl_battle:get_player(PlayerIX, Battle),

   {UpdatedPlayer, PlayerAtaxicUpdate} =
      btl_player:ataxia_set_is_active(false, Player),

   {UpdateBattle, BattleAtaxicUpdate} =
      btl_battle:ataxia_set_player
      (
         PlayerIX,
         UpdatedPlayer,
         PlayerAtaxicUpdate,
         Battle
      ),

   {UpdateBattle, BattleAtaxicUpdate}.

-spec handle_player_defeat
   (
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_player_defeat (PlayerIX, S0Update) ->
   Battle = btl_character_turn_update:get_battle(S0Update),

   {S0Battle, BattleAtaxicUpdate0} =
      mark_characters_of_player_as_defeated(PlayerIX, Battle),
   {S1Battle, BattleAtaxicUpdate1} =
      mark_player_as_inactive(PlayerIX, S0Battle),

   S1Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S1Battle,
         ataxic:sequence([BattleAtaxicUpdate0, BattleAtaxicUpdate1]),
         S0Update
      ),

   S2Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_player_lost(PlayerIX),
         S1Update
      ),

   S2Update.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_character_loss
   (
      btl_character:either(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_character_loss (Character, Update) ->
   Battle = btl_character_turn_update:get_battle(Update),
   Characters = btl_battle:get_characters(Battle),
   CharacterPlayerIX = btl_character:get_player_index(Character),

   StillHasAliveChar =
      lists:any
      (
         fun ({_IX, Char}) ->
            (
               (CharacterPlayerIX == btl_character:get_player_index(Char))
               and btl_character:get_is_alive(Char)
            )
         end,
         orddict:to_list(Characters)
      ),

   case StillHasAliveChar of
      true -> Update;
      _ -> handle_player_defeat(CharacterPlayerIX, Update)
   end.

   %% TODO: Trigger condition: actually dead.

%   TODO: set rank as a condition.
%   case btl_character:get_rank(Character) of
%      optional ->
%         %% Let's not assume there is a commander, meaning that we still have
%         %% to check if at least one character is alive, despite the fact that
%         %% if there is a commander, it being killed would have triggered
%         %% the defeat.
%
%      commander -> handle_player_defeat(CharacterPlayerIX, Update);
%
%      target ->
%         StillHasAliveTargetChar =
%            lists:any
%            (
%               fun ({_IX, Char}) ->
%                  (
%                     (CharacterPlayerIX == btl_character:get_player_index(Char))
%                     and btl_character:get_is_alive(Char)
%                     and (btl_character:get_rank(Char) == target)
%                  )
%               end,
%               orddict:to_list(Characters)
%            ),
%
%         case StillHasAliveTargetChar of
%            true -> Update;
%            _ -> handle_player_defeat(CharacterPlayerIX, Update)
%         end
