-module(btl_turn_actions_attack).
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

-spec handle_attack_sequence
   (
      btl_character:type(),
      non_neg_integer(),
      btl_character:type(),
      non_neg_integer(),
      integer(),
      integer(),
      list(btl_attack:step()),
      list(btl_attack:type())
   )
   ->
   {
      list(btl_attack:type()),
      non_neg_integer(),
      non_neg_integer(),
      integer(),
      integer()
   }.
handle_attack_sequence
(
   _Character,
   CharacterCurrentHealth,
   _TargetCharacter,
   TargetCurrentHealth,
   AttackerLuck,
   DefenderLuck,
   AttackSequence,
   Result
)
when
(
   (CharacterCurrentHealth == 0)
   or (TargetCurrentHealth == 0)
   or (AttackSequence == [])
) ->
   {
      lists:reverse(Result),
      CharacterCurrentHealth,
      TargetCurrentHealth,
      AttackerLuck,
      DefenderLuck
   };
handle_attack_sequence
(
   Character,
   AttackerHealth,
   TargetCharacter,
   DefenderHealth,
   AttackerLuck,
   DefenderLuck,
   [NextAttack | AttackSequence],
   Result
) ->
   AttackEffect =
      btl_attack:get_description_of
      (
         NextAttack,
         btl_character:get_base_character(Character),
         btl_character:get_base_character(TargetCharacter),
         AttackerLuck,
         DefenderLuck
      ),

   {
      AttackResult,
      NewAttackerHealth,
      NewAttackerLuck,
      NewDefenderHealth,
      NewDefenderLuck
   } =
      btl_attack:apply_to_healths_and_lucks
      (
         AttackEffect,
         AttackerHealth,
         AttackerLuck,
         DefenderHealth,
         DefenderLuck
      ),

   NextResult =
      case AttackResult of
         {nothing, _, _} -> Result;
         _ -> [AttackResult|Result]
      end,

   handle_attack_sequence
   (
      Character,
      NewAttackerHealth,
      TargetCharacter,
      NewDefenderHealth,
      NewAttackerLuck,
      NewDefenderLuck,
      AttackSequence,
      NextResult
   ).

-spec get_attack_sequence
   (
      btl_character:type(),
      btl_character:type()
   )
   -> list(btl_attack:step()).
get_attack_sequence (Character, TargetCharacter) ->
   Range =
      shr_location:dist
      (
         btl_character:get_location(Character),
         btl_character:get_location(TargetCharacter)
      ),

   AttackingWeapon =
      shr_character:get_active_weapon
      (
         btl_character:get_base_character(Character)
      ),

   DefendingWeapon =
      shr_character:get_active_weapon
      (
         btl_character:get_base_character(TargetCharacter)
      ),

   btl_attack:get_sequence(Range, AttackingWeapon, DefendingWeapon).

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

   AttackingPlayerIX = btl_character:get_player_index(Character),
   AttackingPlayer = btl_battle:get_player(AttackingPlayerIX, Battle),
   AttackingPlayerLuck = btl_player:get_luck(AttackingPlayer),

   TargetIX = btl_action:get_target_ix(BattleAction),
   Map = btl_battle:get_map(Battle),
   TargetCharacterRef = btl_battle:get_character(TargetIX, Battle),
   TargetCharacter =
      btl_character:resolve
      (
         shr_tile:get_omnimods
         (
            shr_tile:from_id
            (
               shr_tile_instance:get_tile_id
               (
                  shr_map:get_tile_instance
                  (
                     btl_character:get_location(TargetCharacterRef),
                     Map
                  )
               )
            )
         ),
         TargetCharacterRef
      ),

   DefendingPlayerIX = btl_character:get_player_index(TargetCharacter),
   DefendingPlayer = btl_battle:get_player(DefendingPlayerIX, Battle),
   DefendingPlayerLuck = btl_player:get_luck(DefendingPlayer),


   true = btl_character:get_is_alive(TargetCharacter),


   AttackSequence = get_attack_sequence(Character, TargetCharacter),

   {
      AttackEffects,
      RemainingAttackerHealth,
      RemainingDefenderHealth,
      NewAttackerLuck,
      NewDefenderLuck
   } =
      handle_attack_sequence
      (
         Character,
         btl_character:get_current_health(Character),
         TargetCharacter,
         btl_character:get_current_health(TargetCharacter),
         AttackingPlayerLuck,
         DefendingPlayerLuck,
         AttackSequence,
         []
      ),

   S0NewAttackerLuck =
      case {(NewAttackerLuck =< -2), (NewAttackerLuck >= 2)}  of
         {true, _} -> (NewAttackerLuck + 2);
         {_, true} -> (NewAttackerLuck - 2);
         _ -> 0
      end,

   S0NewDefenderLuck =
      case {(NewDefenderLuck =< -2), (NewDefenderLuck >= 2)}  of
         {true, _} -> (NewDefenderLuck + 2);
         {_, true} -> (NewDefenderLuck - 2);
         _ -> 0
      end,

   {UpdatedAttackingPlayer, AttackingPlayerAtaxiaUpdate} =
      btl_player:ataxia_set_luck(S0NewAttackerLuck, AttackingPlayer),

   {UpdatedDefendingPlayer, DefendingPlayerAtaxiaUpdate} =
      btl_player:ataxia_set_luck(S0NewDefenderLuck, DefendingPlayer),

   {UpdatedCharacter, CharacterAtaxiaUpdate} =
      btl_character:ataxia_set_current_health
      (
         RemainingAttackerHealth,
         Character
      ),

   {UpdatedTargetCharacterRef, TargetCharacterRefAtaxiaUpdate} =
      btl_character:ataxia_set_current_health
      (
         RemainingDefenderHealth,
         TargetCharacterRef
      ),

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
         TargetIX,
         UpdatedTargetCharacterRef,
         TargetCharacterRefAtaxiaUpdate,
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
         btl_character_turn_update:get_character_ix(S3Update),
         TargetIX,
         AttackEffects,
         S0NewAttackerLuck,
         S0NewDefenderLuck
      ),

   S4Update = btl_character_turn_update:add_to_timeline(TimelineItem, S3Update),

   S5Update =
      case (RemainingAttackerHealth > 0) of
         true -> S4Update;
         false ->
            btl_victory_progression:handle_character_loss(Character, S4Update)
      end,

   S6Update =
      case (RemainingDefenderHealth > 0) of
         true -> S5Update;
         false ->
            btl_victory_progression:handle_character_loss
            (
               TargetCharacterRef,
               S5Update
            )
      end,

   {ok, S6Update}.
