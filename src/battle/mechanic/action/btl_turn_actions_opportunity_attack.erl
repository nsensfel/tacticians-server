-module(btl_turn_actions_opportunity_attack).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> {ok, btl_character_turn_update:type()}.
handle (BattleAction, Update) ->
   {S0Update, Battle} = btl_character_turn_update:get_battle(Update),
   {S1Update, Character} = btl_character_turn_update:get_character(S0Update),

   DefendingPlayerIX = btl_character:get_player_index(Character),
   DefendingPlayer = btl_battle:get_player(DefendingPlayerIX, Battle),
   DefendingPlayerLuck = btl_player:get_luck(DefendingPlayer),

   AttackerIX = btl_action:get_target_ix(BattleAction),
   AttackerRef = btl_battle:get_character(AttackerIX, Battle),
   Attacker = btl_battle:resolve_character(AttackerRef, Battle),

   AttackingPlayerIX = btl_character:get_player_index(Attacker),
   AttackingPlayer = btl_battle:get_player(AttackingPlayerIX, Battle),
   AttackingPlayerLuck = btl_player:get_luck(AttackingPlayer),

   Attack = btl_attack:attack_of_opportunity(),

   AttackEffect =
      btl_attack:get_description_of
      (
         Attack,
         btl_character:get_base_character(Character),
         btl_character:get_base_character(Attacker),
         AttackingPlayerLuck,
         DefendingPlayerLuck
      ),

   {
      AttackResult,
      NewAttackerHealth,
      S0NewAttackerLuck,
      NewDefenderHealth,
      S0NewDefenderLuck
   } =
      btl_attack:apply_to_healths_and_lucks
      (
         AttackEffect,
         btl_character:get_current_health(Attacker),
         AttackingPlayerLuck,
         btl_character:get_current_health(Character),
         DefendingPlayerLuck
      ),

   S1NewAttackerLuck =
      case {(S0NewAttackerLuck =< -2), (S0NewAttackerLuck >= 2)}  of
         {true, _} -> (S0NewAttackerLuck + 2);
         {_, true} -> (S0NewAttackerLuck - 2);
         _ -> 0
      end,

   S1NewDefenderLuck =
      case {(S0NewDefenderLuck =< -2), (S0NewDefenderLuck >= 2)}  of
         {true, _} -> (S0NewDefenderLuck + 2);
         {_, true} -> (S0NewDefenderLuck - 2);
         _ -> 0
      end,

   {UpdatedAttackingPlayer, AttackingPlayerAtaxiaUpdate} =
      btl_player:ataxia_set_luck(S1NewAttackerLuck, AttackingPlayer),

   {UpdatedDefendingPlayer, DefendingPlayerAtaxiaUpdate} =
      btl_player:ataxia_set_luck(S1NewDefenderLuck, DefendingPlayer),

   {UpdatedCharacter, CharacterAtaxiaUpdate} =
      btl_character:ataxia_set_current_health(NewDefenderHealth, Character),

   {UpdatedAttackerRef, AttackerRefAtaxiaUpdate} =
      btl_character:ataxia_set_current_health(NewAttackerHealth, AttackerRef),

   {S0Battle, BattleAtaxiaUpdate0} =
      btl_battle:ataxia_set_player
      (
         AttackingPlayerIX,
         UpdatedAttackingPlayer,
         AttackingPlayerAtaxiaUpdate,
         Battle
      ),

   {S1Battle, BattleAtaxiaUpdate1} =
      btl_battle:ataxia_set_player
      (
         DefendingPlayerIX,
         UpdatedDefendingPlayer,
         DefendingPlayerAtaxiaUpdate,
         S0Battle
      ),

   {S2Battle, BattleAtaxiaUpdate2} =
      btl_battle:ataxia_set_character
      (
         AttackerIX,
         UpdatedAttackerRef,
         AttackerRefAtaxiaUpdate,
         S1Battle
      ),

   % Potential danger ahead: we're going to update both the 'character' and
   % 'battle' members of a btl_character_turn_update.
   % 'S1Update' is sure to have both up to date (as it's the result of 'get'
   % requests for both) and there is no risk of the 'battle' update influencing
   % 'character', making what follows safe.

   S2Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S2Battle,
         false,
         ataxic:optimize
         (
            ataxic:sequence
            (
               [
                  BattleAtaxiaUpdate0,
                  BattleAtaxiaUpdate1,
                  BattleAtaxiaUpdate2
               ]
            )
         ),
         S1Update
      ),

   S3Update =
      btl_character_turn_update:ataxia_set_character
      (
         UpdatedCharacter,
         CharacterAtaxiaUpdate,
         S2Update
      ),

   TimelineItem =
      btl_turn_result:new_character_attacked
      (
         AttackerIX,
         btl_character_turn_update:get_character_ix(S3Update),
         AttackResult,
         S0NewAttackerLuck,
         S0NewDefenderLuck
      ),

   S4Update = btl_character_turn_update:add_to_timeline(TimelineItem, S3Update),

   S5Update =
      case (NewDefenderHealth > 0) of
         true -> S4Update;
         false ->
            btl_victory_progression:handle_character_loss(Character, S4Update)
      end,

   {ok, S5Update}.
