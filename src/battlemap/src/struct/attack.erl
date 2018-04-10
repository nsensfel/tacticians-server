-module(attack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type order() :: ('first' | 'second' | 'counter').
-type precision() :: ('misses' | 'grazes' | 'hits').

-record
(
   attack,
   {
      order :: order(),
      precision :: precision(),
      is_critical :: boolean(),
      is_parry :: boolean(),
      damage :: non_neg_integer()
   }
).

-opaque struct() :: #attack{}.
-type maybe_struct() :: ('nothing' | struct()).
-opaque step() :: {order(), boolean()}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([struct/0, maybe_struct/0, step/0]).

-export
(
   [
      get_sequence/3,
      get_description_of/3,
      apply_to_healths/3
   ]
).

-export
(
   [
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec roll_precision
   (
      statistics:struct(),
      statistics:struct()
   )
   -> precision().
roll_precision (AttackerStatistics, DefenderStatistics) ->
   DefenderDodges = statistics:get_dodges(DefenderStatistics),
   AttackerAccuracy = statistics:get_accuracy(AttackerStatistics),
   MissChance = max(0, (DefenderDodges - AttackerAccuracy)),
   case roll:percentage() of
      X when (X =< MissChance) -> misses;
      X when (X =< (MissChance * 2)) -> grazes;
      _ -> hits
   end.

-spec roll_damage
   (
      statistics:struct(),
      statistics:struct()
   )
   -> {non_neg_integer(), boolean()}.
roll_damage (AttackerStatistics, _DefenderStatistics) ->
   {MinimumDamage, MaximumDamage} = statistics:get_damages(AttackerStatistics),
   MaximumRoll = max(1, MaximumDamage - MinimumDamage),
   BaseDamage = MinimumDamage + (rand:uniform(MaximumRoll) - 1),
   CriticalHitChance = statistics:get_critical_hits(AttackerStatistics),
   case roll:percentage() of
      X when (X =< CriticalHitChance) -> {(BaseDamage * 2), true};
      _ -> {BaseDamage, false}
   end.

-spec roll_parry (statistics:struct()) -> boolean().
roll_parry (DefenderStatistics) ->
   DefenderParryChance = statistics:get_parries(DefenderStatistics),
   (roll:percentage() =< DefenderParryChance).

-spec effect_of_attack
   (
      order(),
      statistics:struct(),
      statistics:struct(),
      boolean()
   )
   -> struct().
effect_of_attack (Order, AttackerStatistics, DefenderStatistics, CanParry) ->
   ParryIsSuccessful = (CanParry and roll_parry(DefenderStatistics)),
   {ActualAtkStatistics, ActualDefStatistics} =
      case ParryIsSuccessful of
         true -> {DefenderStatistics, AttackerStatistics};
         false -> {AttackerStatistics, DefenderStatistics}
      end,

   Precision = roll_precision(ActualAtkStatistics, ActualDefStatistics),
   {Damage, IsCritical} = roll_damage(ActualAtkStatistics, ActualDefStatistics),
   ActualDamage =
      case Precision of
         misses -> 0;
         grazes -> trunc(Damage / 2);
         hits -> Damage
      end,

   #attack
   {
      order = Order,
      precision = Precision,
      is_critical = IsCritical,
      is_parry = ParryIsSuccessful,
      damage = ActualDamage
   }.

-spec encode_order (order()) -> binary().
encode_order (first) -> <<"f">>;
encode_order (counter) -> <<"c">>;
encode_order (second) -> <<"s">>.

-spec encode_precision (precision()) -> binary().
encode_precision (hits) -> <<"h">>;
encode_precision (grazes) -> <<"g">>;
encode_precision (misses) -> <<"m">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_description_of
   (
      step(),
      statistics:struct(),
      statistics:struct()
   )
   -> maybe_struct().
get_description_of
(
   {first, CanParry},
   AttackerStatistics,
   DefenderStatistics
) ->
   effect_of_attack(first, AttackerStatistics, DefenderStatistics, CanParry);
get_description_of
(
   {second, CanParry},
   AttackerStatistics,
   DefenderStatistics
) ->
   AttackerDoubleAttackChange = statistics:get_double_hits(AttackerStatistics),

   case roll:percentage() of
      X when (X =< AttackerDoubleAttackChange) ->
         effect_of_attack
         (
            second,
            AttackerStatistics,
            DefenderStatistics,
            CanParry
         );

      _ ->
         nothing
   end;
get_description_of
(
   {counter, CanParry},
   AttackerStatistics,
   DefenderStatistics
) ->
   effect_of_attack(counter, DefenderStatistics, AttackerStatistics, CanParry).

-spec apply_to_healths
   (
      maybe_struct(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> {maybe_struct(), non_neg_integer(), non_neg_integer()}.
apply_to_healths
(
   nothing,
   AttackerHealth,
   DefenderHealth
) ->
   {nothing, AttackerHealth, DefenderHealth};
apply_to_healths
(
   Attack,
   AttackerHealth,
   DefenderHealth
)
when
(
   (Attack#attack.order == first)
   or (Attack#attack.order == second)
   or ((Attack#attack.order == counter) and Attack#attack.is_parry)
) ->
   Damage = Attack#attack.damage,

   case AttackerHealth of
      0 ->
         {nothing, AttackerHealth, DefenderHealth};

      _ ->
         {
            Attack,
            AttackerHealth,
            max(0, (DefenderHealth - Damage))
         }
   end;
apply_to_healths
(
   Attack,
   AttackerHealth,
   DefenderHealth
)
when
(
   (Attack#attack.order == counter)
   or
   (
      (Attack#attack.is_parry)
      and ((Attack#attack.order == first) or (Attack#attack.order == second))
   )
) ->
   Damage = Attack#attack.damage,

   case DefenderHealth of
      0 ->
         {nothing, AttackerHealth, DefenderHealth};

      _ ->
         {
            Attack,
            max(0, (AttackerHealth - Damage)),
            DefenderHealth
         }
   end.

-spec get_sequence
   (
      non_neg_integer(),
      weapon:struct(),
      weapon:struct()
   )
   -> list(step()).
get_sequence (AttackRange, AttackerWeapon, DefenderWeapon) ->
   {AttackerDefenseRange, AttackerAttackRange} =
      weapon:get_ranges(AttackerWeapon),
   {DefenderDefenseRange, DefenderAttackRange} =
      weapon:get_ranges(DefenderWeapon),

   AttackerCanAttack = (AttackRange =< AttackerAttackRange),
   AttackerCanAttack = true,
   AttackerCanDefend =
      (AttackerCanAttack and (AttackRange > AttackerDefenseRange)),
   AttackerCanParry =
      (AttackerCanDefend and weapon:can_parry(AttackerWeapon)),

   DefenderCanAttack = (AttackRange =< DefenderAttackRange),
   DefenderCanDefend =
      (DefenderCanAttack and (AttackRange > DefenderDefenseRange)),
   DefenderCanParry =
      (DefenderCanDefend and weapon:can_parry(DefenderWeapon)),

   First = {first, DefenderCanParry},
   Second = {second, DefenderCanParry},
   Counter = {counter, AttackerCanParry},

   if
      (not DefenderCanDefend) ->
         [First, Second];

      true ->
         [First, Counter, Second]
   end.

-spec encode (struct()) -> {list(any())}.
% This shouldn't be a possibility. Types in this module are a mess...
encode (Attack) ->
   Order = Attack#attack.order,
   Precision = Attack#attack.precision,
   IsCritical = Attack#attack.is_critical,
   IsParry = Attack#attack.is_parry,
   Damage = Attack#attack.damage,

   {
      [
         {<<"ord">>, encode_order(Order)},
         {<<"pre">>, encode_precision(Precision)},
         {<<"cri">>, IsCritical},
         {<<"par">>, IsParry},
         {<<"dmg">>, Damage}
      ]
   }.
