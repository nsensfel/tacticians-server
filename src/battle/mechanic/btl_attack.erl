-module(btl_attack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec roll_precision
   (
      shr_statistics:type(),
      shr_statistics:type(),
      integer()
   )
   -> {precision(), integer(), integer()}.
roll_precision
(
   AttackerStatistics,
   DefenderStatistics,
   DefenderLuck
) ->
   DefenderDodges = shr_statistics:get_dodges(DefenderStatistics),
   AttackerAccuracy = shr_statistics:get_accuracy(AttackerStatistics),
   MissChance = max(0, (DefenderDodges - AttackerAccuracy)),

   {Roll, _IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(MissChance, DefenderLuck),

   {
      case Roll of
         X when (X =< MissChance) -> misses;
         X when (X =< (MissChance * 2)) -> grazes;
         _ -> hits
      end,
      PositiveModifier,
      NegativeModifier
   }.

-spec roll_critical_hit
   (
      shr_statistics:type(),
      integer()
   )
   -> {boolean(), integer(), integer()}.
roll_critical_hit (AttackerStatistics, AttackerLuck) ->
   CriticalHitChance = shr_statistics:get_critical_hits(AttackerStatistics),
   {_Roll, IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(CriticalHitChance, AttackerLuck),

   {IsSuccess, PositiveModifier, NegativeModifier}.

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

-spec effect_of_attack
   (
      order(),
      shr_character:type(),
      shr_character:type(),
      boolean(),
      integer(),
      integer()
   )
   -> type().
effect_of_attack
(
   Order,
   Attacker,
   Defender,
   CanParry,
   AttackerLuck,
   DefenderLuck
) ->
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%% Roll parry to see if the roles have to be swapped. %%%%%%%%%%%%%%%%%%%%%

   DefenderStats = shr_character:get_statistics(Defender),
   {ParryIsSuccessful, ParryPositiveLuckMod, ParryNegativeLuckMod} =
      case CanParry of
         true -> roll_parry(DefenderStats, DefenderLuck);
         false -> {false, 0, 0}
      end,

   {
      ActualAttacker,
      ActualDefender,
      ActualAttackerLuck,
      ActualDefenderLuck
   } =
      case ParryIsSuccessful of
         true -> {Defender, Attacker, DefenderLuck, AttackerLuck};
         false -> {Attacker, Defender, AttackerLuck, DefenderLuck}
      end,

   ActualAttackerStats = shr_character:get_statistics(ActualAttacker),
   ActualAttackerOmnimods = shr_character:get_omnimods(ActualAttacker),
   ActualDefenderStats = shr_character:get_statistics(ActualDefender),
   ActualDefenderOmnimods = shr_character:get_omnimods(ActualDefender),

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   {Precision, PrecisionPositiveLuckMod, PrecisionNegativeLuckMod} =
      roll_precision
      (
         ActualAttackerStats,
         ActualDefenderStats,
         ActualDefenderLuck
      ),


   {IsCritical, CriticalPositiveLuckMod, CriticalNegativeLuckMod} =
      roll_critical_hit(ActualAttackerStats, ActualAttackerLuck),

   ActualAttackerDamageModifier =
      shr_statistics:get_damage_modifier(ActualAttackerStats),

   Damage =
      get_damage
      (
         Precision,
         IsCritical,
         ActualAttackerDamageModifier,
         ActualAttackerOmnimods,
         ActualDefenderOmnimods
      ),

   {FinalAttackerLuckMod, FinalDefenderLuckMod} =
      case {ParryIsSuccessful, Precision} of
         {true, misses} ->
            {
               (
                  % Attacker wasn't the one parrying
                  ParryNegativeLuckMod
                  % Attacker was the one evading
                  + PrecisionPositiveLuckMod
                  % miss -> no critical hit luck modifier
               ),
               (
                  % Defender was the one parrying
                  ParryPositiveLuckMod
                  % Defender wasn't the one evading
                  + PrecisionNegativeLuckMod
                  % miss -> no critical hit luck modifier
               )
            };

         {true, _} ->
            {
               (
                  % Attacker wasn't the one parrying
                  ParryNegativeLuckMod
                  % Attacker was the one evading
                  + PrecisionPositiveLuckMod
                  % Attacker wasn't the one doing a critical
                  + CriticalNegativeLuckMod
               ),
               (
                  % Defender was the one parrying
                  ParryPositiveLuckMod
                  % Defender wasn't the one evading
                  + PrecisionNegativeLuckMod
                  % Defender was the one doing a critical
                  + CriticalPositiveLuckMod
               )
            };

         {false, misses} ->
            {
               (
                  % Attacker wasn't the one parrying
                  ParryNegativeLuckMod
                  % Defender was the one evading
                  + PrecisionNegativeLuckMod
                  % miss -> no critical hit luck modifier
               ),
               (
                  % Defender was the one parrying
                  ParryPositiveLuckMod
                  % Defender was the one evading
                  + PrecisionPositiveLuckMod
                  % miss -> no critical hit luck modifier
               )
            };

         {false, _} ->
            {
               (
                  % Attacker wasn't the one parrying
                  ParryNegativeLuckMod
                  % Attacker wasn't the one evading
                  + PrecisionNegativeLuckMod
                  % Attacker was the one doing a critical
                  + CriticalPositiveLuckMod
               ),
               (
                  % Defender was the one parrying
                  ParryPositiveLuckMod
                  % Defender was the one evading
                  + PrecisionPositiveLuckMod
                  % Defender wasn't the one doing a critical
                  + CriticalNegativeLuckMod
               )
            }
      end,

   #attack
   {
      order = Order,
      precision = Precision,
      is_critical = IsCritical,
      is_parry = ParryIsSuccessful,
      damage = Damage,
      attacker_luck_mod = FinalAttackerLuckMod,
      defender_luck_mod = FinalDefenderLuckMod
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_description_of
   (
      step(),
      shr_character:type(),
      shr_character:type(),
      integer(),
      integer()
   )
   -> maybe_type().
get_description_of
(
   {first, CanParry},
   Attacker,
   Defender,
   AttackerLuck,
   DefenderLuck
) ->
   effect_of_attack
   (
      first,
      Attacker,
      Defender,
      CanParry,
      AttackerLuck,
      DefenderLuck
   );
get_description_of
(
   {second, CanParry},
   Attacker,
   Defender,
   AttackerLuck,
   DefenderLuck
) ->
   AttackerStats = shr_character:get_statistics(Attacker),
   AttackerDoubleAttackChance =
      shr_statistics:get_double_hits(AttackerStats),
   {_Roll, IsSuccessful, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(AttackerDoubleAttackChance, AttackerLuck),

   NewAttackerLuck = (AttackerLuck + PositiveModifier),
   NewDefenderLuck = (DefenderLuck + NegativeModifier),

   case IsSuccessful of
      true ->
         Result =
            effect_of_attack
            (
               second,
               Attacker,
               Defender,
               CanParry,
               NewAttackerLuck,
               NewDefenderLuck
            ),

         Result#attack
         {
            attacker_luck_mod =
               (Result#attack.attacker_luck_mod + PositiveModifier),
            defender_luck_mod =
               (Result#attack.defender_luck_mod + NegativeModifier)
         };

      _ -> {nothing, PositiveModifier, NegativeModifier}
   end;
get_description_of
(
   {counter, CanParry},
   Attacker,
   Defender,
   AttackerLuck,
   DefenderLuck
) ->
   effect_of_attack
   (
      counter,
      Defender,
      Attacker,
      CanParry,
      DefenderLuck,
      AttackerLuck
   ).

-spec apply_to_healths_and_lucks
   (
      maybe_type(),
      non_neg_integer(),
      integer(),
      non_neg_integer(),
      integer()
   )
   ->
   {
      maybe_type(),
      non_neg_integer(),
      integer(),
      non_neg_integer(),
      integer()
   }.
apply_to_healths_and_lucks
(
   _Attack,
   AttackerHealth,
   AttackerLuck,
   DefenderHealth,
   DefenderLuck
)
when
(
   (AttackerHealth =< 0)
   or (DefenderHealth =< 0)
) ->
   {
      {nothing, 0, 0},
      AttackerHealth,
      AttackerLuck,
      DefenderHealth,
      DefenderLuck
   };
apply_to_healths_and_lucks
(
   {nothing, AttackerLuckMod, DefenderLuckMod},
   AttackerHealth,
   AttackerLuck,
   DefenderHealth,
   DefenderLuck
) ->
   {
      {nothing, AttackerLuckMod, DefenderLuckMod},
      AttackerHealth,
      (AttackerLuck + AttackerLuckMod),
      DefenderHealth,
      (DefenderLuck + DefenderLuckMod)
   };
apply_to_healths_and_lucks
(
   Attack,
   AttackerHealth,
   AttackerLuck,
   DefenderHealth,
   DefenderLuck
)
when
(
   (
      (not Attack#attack.is_parry)
      and ((Attack#attack.order == first) or (Attack#attack.order == second))
   )
   or
   (
      Attack#attack.is_parry
      and (Attack#attack.order == counter)
   )
) ->
   Damage = Attack#attack.damage,

   {
      Attack,
      AttackerHealth,
      (AttackerLuck + Attack#attack.attacker_luck_mod),
      (DefenderHealth - Damage),
      (DefenderLuck + Attack#attack.defender_luck_mod)
   };
apply_to_healths_and_lucks
(
   Attack,
   AttackerHealth,
   AttackerLuck,
   DefenderHealth,
   DefenderLuck
)
when
(
   (
      (not Attack#attack.is_parry)
      and (Attack#attack.order == counter)
   )
   or
   (
      Attack#attack.is_parry
      and ((Attack#attack.order == first) or (Attack#attack.order == second))
   )
) ->
   Damage = Attack#attack.damage,

   {
      Attack,
      (AttackerHealth - Damage),
      (AttackerLuck + Attack#attack.attacker_luck_mod),
      DefenderHealth,
      (DefenderLuck + Attack#attack.defender_luck_mod)
   }.

-spec get_sequence
   (
      non_neg_integer(),
      shr_weapon:type(),
      shr_weapon:type()
   )
   -> list(step()).
get_sequence (AttackRange, AttackerWeapon, DefenderWeapon) ->
   AttackerDefenseRange = shr_weapon:get_minimum_range(AttackerWeapon),
   AttackerAttackRange =  shr_weapon:get_maximum_range(AttackerWeapon),
   DefenderDefenseRange = shr_weapon:get_minimum_range(DefenderWeapon),
   DefenderAttackRange =  shr_weapon:get_maximum_range(DefenderWeapon),

   AttackerCanAttack = (AttackRange =< AttackerAttackRange),
   AttackerCanDefend =
      (AttackerCanAttack and (AttackRange > AttackerDefenseRange)),

   true = (AttackerCanAttack == true),

   DefenderCanAttack = (AttackRange =< DefenderAttackRange),
   DefenderCanDefend =
      (DefenderCanAttack and (AttackRange > DefenderDefenseRange)),

   First = {first, DefenderCanDefend},
   Second = {second, DefenderCanDefend},
   Counter = {counter, AttackerCanDefend},

   case DefenderCanDefend of
      true -> [First, Counter, Second];
      _ -> [First, Second]
   end.

-spec standard
   (
      non_neg_integer(),
      btl_character:type(),
      btl_player:type(),
      btl_character:type()
      btl_player:type()
   )
   ->
   {
      % Attack Descriptions,
      % Updated Attacker
      % Attacker Ataxia Update
      % Updated Defender
      % Defender Ataxia Update
   }

-spec attack_of_opportunity
   (
      btl_character:type(),
      btl_player:type(),
      btl_character:type()
      btl_player:type(),
   )
   ->
   {
      % Attack Descriptions,
      % Updated Attacker
      % Attacker Ataxia Update
      % Updated Attacking Player
      % Attacking Player Ataxia Update
      % Updated Defender
      % Defender Ataxia Update
      % Updated Defending Player
      % Attacking Player Ataxia Update
   }

attack_of_opportunity () ->
   [
      {first, false},
      {second, false}
   ].
