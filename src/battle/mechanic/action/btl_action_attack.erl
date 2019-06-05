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
      handle/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec roll_precision_modifier
   (
      shr_statistics:type(),
      shr_statistics:type(),
      integer()
   )
   -> {float(), integer(), integer()}.
roll_precision_modifier (Statistics, TargetStatistics, TargetLuck) ->
   TargetDodges = shr_statistics:get_dodges(TargetStatistics),
   Accuracy = shr_statistics:get_accuracy(Statistics),
   MissChance = max(0, (TargetDodges - Accuracy)),

   {Roll, _IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(MissChance, TargetLuck),

   {
      case Roll of
         X when (X =< MissChance) -> 0.0;
         X when (X =< (MissChance * 2)) -> 0.5;
         _ -> 1.0
      end,
      PositiveModifier,
      NegativeModifier
   }.

-spec roll_critical_hit_modifier
   (
      shr_statistics:type(),
      integer()
   )
   -> {boolean(), integer(), integer()}.
roll_critical_hit_modifier (Statistics, Luck) ->
   CriticalHitChance = shr_statistics:get_critical_hits(Statistics),
   {_Roll, IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(CriticalHitChance, Luck),

   {
      case IsSuccess of
         true -> 2.0; % [TODO][FUTURE]: variable critical multiplier?
         false -> 1.0
      end,
      PositiveModifier,
      NegativeModifier
   }.

-spec roll_parry
   (
      shr_statistics:type(),
      integer()
   )
   -> {boolean(), integer(), integer()}.
roll_parry (DefenderStatistics, DefenderLuck) ->
   DefenderParryChance = shr_statistics:get_parries(DefenderStatistics),
   {_Roll, IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(DefenderParryChance, DefenderLuck),

   {IsSuccess, PositiveModifier, NegativeModifier}.

-spec get_damage
   (
      precision(),
      boolean(),
      float(),
      shr_omnimods:type(),
      shr_omnimods:type()
   )
   -> non_neg_integer().
get_damage
(
   Precision,
   IsCritical,
   StartingDamageMultiplier,
   AttackerOmnimods,
   DefenderOmnimods
) ->
   ActualDamageMultiplier =
      (
         StartingDamageMultiplier
         *
         (
            case Precision of
               misses -> 0;
               grazes -> 0.5;
               hits -> 1
            end
         )
         *
         (
            case IsCritical of
               true -> 2;
               _ -> 1
            end
         )
      ),

   ActualDamage =
      shr_omnimods:get_attack_damage
      (
         ActualDamageMultiplier,
         AttackerOmnimods,
         DefenderOmnimods
      ),

   ActualDamage.

-spec get_character_abilities
   (
      btl_action:type(),
      btl_character:type(),
      btl_character:type()
   )
   -> {boolean(), boolean(), boolean()}.
get_character_abilities (Action, Character, TargetCharacter) ->
   CharacterWeapon =
      shr_character:get_active_weapon
      (
         btl_character:get_base_character(Character)
      ),

   TargetCharacterWeapon =
      shr_character:get_active_weapon
      (
         btl_character:get_base_character(TargetCharacter)
      ),

   DefenseRange = shr_weapon:get_minimum_range(CharacterWeapon),
   AttackRange =  shr_weapon:get_maximum_range(CharacterWeapon),
   TargetDefenseRange = shr_weapon:get_minimum_range(TargetCharacterWeapon),
   TargetAttackRange =  shr_weapon:get_maximum_range(TargetCharacterWeapon),

   IsNotOpportunistic = btl_action:get_is_opportunistic(Action),

   AttackRange =
      shr_location:dist
      (
         btl_character:get_location(Character),
         btl_character:get_location(TargetCharacter)
      ),

   {
      (DefenseRange == 0),
      (
         IsNotOpportunistic
         and (TargetDefenseRange == 0)
         and (TargetAttackRange =< AttackRange)
      ),
      (
         IsNotOpportunistic
         and (TargetAttackRange =< AttackRange)
      )
   }.

-spec effect_of_attack
   (
      btl_attack:category(),
      non_neg_integer(),
      non_neg_integer(),
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      boolean(),
      btl_character_turn_update:type()
   )
   ->
   {
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      btl_character_turn_update:type(),
      btl_attack:type()
   }.
effect_of_attack
(
   Category,
   CharacterIX,
   TargetCharacterIX,
   S0Character,
   S0TargetCharacter,
   S0Luck,
   S0TargetLuck,
   TargetCanParry,
   S0Update
) ->
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%% Roll parry to see if the roles have to be swapped. %%%%%%%%%%%%%%%%%%%%%

   {ParryIsSuccessful, ParryPositiveLuckMod, ParryNegativeLuckMod} =
      case TargetCanParry of
         true ->
            TargetStatistics =
               shr_character:get_statistics
               (
                  btl_character:get_base_character(TargetCharacter)
               ),
            roll_parry(TargetStatistics, S0TargetLuck);

         false -> {false, 0, 0}
      end,

   { AttackerIX, DefenderIX, Attacker, Defender, AttackerLuck, DefenderLuck } =
      case ParryIsSuccessful of
         true ->
            {
               TargetCharacterIX,
               CharacterIX,
               TargetCharacter,
               Character,
               TargetLuck,
               Luck
            };

         false ->
            {
               CharacterIX,
               TargetCharacterIX,
               Character,
               TargetCharacter,
               Luck,
               TargetLuck
            }
      end,

   AttackerStatistics =

-spec handle_attack_sequence
   (
      list({btl_attack:category(), boolean()}),
      non_neg_integer(),
      non_neg_integer(),
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      list(btl_attack:type()),
      btl_character_turn_update:type()
   )
   ->
   {
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      list(btl_attack:type()),
      btl_character_turn_update:type()
   }.
handle_attack_sequence
(
   [],
   _CharacterIX,
   _TargetCharacterIX,
   Character,
   TargetCharacter,
   PlayerLuck,
   TargetPlayerLuck,
   Results,
   Update
)
->
   {
      Character,
      TargetCharacter,
      PlayerLuck,
      TargetPlayerLuck,
      lists:reverse(Results),
      Update
   };
handle_attack_sequence
(
   [{first, TargetCanParry}|NextAttacks],
   CharacterIX,
   TargetCharacterIX,
   S0Character,
   S0TargetCharacter,
   S0PlayerLuck,
   S0TargetPlayerLuck,
   Results,
   S0Update
)
->
   {
      S1Character,
      S1TargetCharacter,
      S1PlayerLuck,
      S1TargetPlayerLuck,
      S1Update,
      Result
   } =
      effect_of_attack
      (
         first,
         CharacterIX,
         TargetCharacterIX,
         S0Character,
         S0TargetCharacter,
         S0PlayerLuck,
         S0TargetPlayerLuck,
         TargetCanParry,
         S0Update
      ),

   handle_attack_sequence
   (
      NextAttacks,
      CharacterIX,
      TargetCharacterIX,
      S1Character,
      S1TargetCharacter,
      S1PlayerLuck,
      S1TargetPlayerLuck,
      [Result|Results],
      S1Update
   );
handle_attack_sequence
(
   [{counter, CanParry}|NextAttacks],
   CharacterIX,
   TargetCharacterIX,
   S0Character,
   S0TargetCharacter,
   S0PlayerLuck,
   S0TargetPlayerLuck,
   Results,
   S0Update
)
->
   {
      S1TargetCharacter,
      S1Character,
      S2TargetPlayerLuck,
      S2PlayerLuck,
      S1Update,
      Result
   } =
      effect_of_attack
      (
         counter,
         TargetCharacterIX,
         CharacterIX,
         S0TargetCharacter,
         S0Character,
         S1TargetPlayerLuck,
         S1PlayerLuck,
         CanParry,
         S0Update
      ),

   handle_attack_sequence
   (
      NextAttacks,
      CharacterIX,
      TargetCharacterIX,
      S1Character,
      S1TargetCharacter,
      S2PlayerLuck,
      S2TargetPlayerLuck,
      [Result|Results],
      S1Update
   );
handle_attack_sequence
(
   [{second, TargetCanParry}|NextAttacks],
   CharacterIX,
   TargetCharacterIX,
   S0Character,
   S0TargetCharacter,
   S0PlayerLuck,
   S0TargetPlayerLuck,
   Results,
   S0Update
)
->
   Statistics = shr_character:get_statistics(S0Character),
   DoubleAttackChance = shr_statistics:get_double_hits(Statistics),
   {_Roll, IsSuccessful, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(DoubleAttackChance, S0PlayerLuck),

   S1PlayerLuck = (S0PlayerLuck + PositiveModifier),
   S1TargetPlayerLuck = (S0TargetPlayerLuck + NegativeModifier),

   case IsSuccessful of
      false ->
         handle_attack_sequence
         (
            NextAttacks,
            CharacterIX,
            TargetCharacterIX,
            S0Character,
            S0TargetCharacter,
            S1PlayerLuck,
            S1TargetPlayerLuck,
            Results,
            S0Update
         );

      true ->
         {
            S1Character,
            S1TargetCharacter,
            S2PlayerLuck,
            S2TargetPlayerLuck,
            S1Update,
            Result
         } =
            effect_of_attack
            (
               second,
               CharacterIX,
               TargetCharacterIX,
               S0Character,
               S0TargetCharacter,
               S1PlayerLuck,
               S1TargetPlayerLuck,
               TargetCanParry,
               S0Update
            ),

         handle_attack_sequence
         (
            CharacterIX,
            TargetCharacterIX,
            NextAttacks,
            S1Character,
            S1TargetCharacter,
            S2PlayerLuck,
            S2TargetPlayerLuck,
            [Result|Results],
            S1Update
         )
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_action:type(),
      btl_character:type(),
      btl_character_turn_update:type()
   )
   -> {ok, btl_character_turn_update:type()}.
handle (Action, S0Character, S0Update) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),
   CharacterIX = btl_action:get_actor_index(Action),

   PlayerIX = btl_character:get_player_index(S0Character),
   Player = btl_battle:get_player(PlayerIX, S0Battle),
   S0PlayerLuck = btl_player:get_luck(Player),

   TargetCharacterIX = btl_action:get_target_index(Action),
   {S0TargetCharacter, S1Battle} =
      btl_battle:get_resolved_character(TargetCharacterIX, S0Battle),

   TargetPlayerIX = btl_character:get_player_index(TargetCharacter),
   TargetPlayer = btl_battle:get_player(TargetPlayerIX, S1Battle),
   TargetPlayerLuck = btl_player:get_luck(TargetPlayer),

   {CanParry, TargetCanParry, TargetCanCounter} =
      get_character_abilities(Action, S0Character, S0TargetCharacter),

   {
      S1Character,
      S1TargetCharacter,
      S1PlayerLuck,
      S1TargetPlayerLuck,
      Results,
      S1Update
   } =
      handle_attack_sequence
      (
         case TargetCanCounter of
            true ->
               [
                  {first, TargetCanParry},
                  {counter, CanParry},
                  {second, TargetCanParry}
               ];

            false ->
               [
                  {first, TargetCanParry},
                  {second, TargetCanParry}
               ]
         end,
         S1Character,
         S1TargetCharacter,
         S1PlayerLuck,
         S1TargetPlayerLuck,
         Results,
         S1Update
      ),

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
         PlayerLuck,
         TargetPlayerLuck,
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
