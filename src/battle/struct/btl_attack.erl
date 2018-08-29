-module(btl_attack).

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

-opaque type() :: #attack{}.
-type maybe_type() :: ('nothing' | type()).
-opaque step() :: {order(), boolean()}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0, maybe_type/0, step/0]).

-export
(
   [
      get_sequence/3,
      get_description_of/5,
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
      shr_statistics:type(),
      shr_statistics:type()
   )
   -> precision().
roll_precision (AttackerStatistics, DefenderStatistics) ->
   DefenderDodges = shr_statistics:get_dodges(DefenderStatistics),
   AttackerAccuracy = shr_statistics:get_accuracy(AttackerStatistics),
   MissChance = max(0, (DefenderDodges - AttackerAccuracy)),
   case shr_roll:percentage() of
      X when (X =< MissChance) -> misses;
      X when (X =< (MissChance * 2)) -> grazes;
      _ -> hits
   end.

-spec roll_critical_hit (shr_statistics:type()) -> boolean().
roll_critical_hit (AttackerStatistics) ->
   CriticalHitChance = shr_statistics:get_critical_hits(AttackerStatistics),
   (shr_roll:percentage() =< CriticalHitChance).

-spec roll_parry (shr_statistics:type()) -> boolean().
roll_parry (DefenderStatistics) ->
   DefenderParryChance = shr_statistics:get_parries(DefenderStatistics),
   (shr_roll:percentage() =< DefenderParryChance).

-spec effect_of_attack
   (
      order(),
      shr_statistics:type(),
      shr_omnimods:type(),
      shr_statistics:type(),
      shr_omnimods:type(),
      boolean()
   )
   -> type().
effect_of_attack (Order, AtkStats, AtkOmni, DefStats, DefOmni, CanParry) ->
   ParryIsSuccessful = (CanParry and roll_parry(DefStats)),

   {ActualAtkStats, ActualDefStats} =
      case ParryIsSuccessful of
         true -> {DefStats, AtkStats};
         false -> {AtkStats, DefStats}
      end,

   {ActualAtkOmni, ActualDefOmni} =
      case ParryIsSuccessful of
         true -> {DefOmni, AtkOmni};
         false -> {AtkOmni, DefOmni}
      end,

   Precision = roll_precision(ActualAtkStats, ActualDefStats),
   IsCritical = roll_critical_hit(ActualAtkStats),

   S0DamageMultiplier =
      case Precision of
         misses -> 0;
         grazes -> 0.5;
         hits -> 1
      end,

   S1DamageMultiplier =
      case IsCritical of
         true -> (S0DamageMultiplier * 2);
         false -> S0DamageMultiplier
      end,

   ActualDamage =
      shr_omnimods:get_attack_damage
      (
         S1DamageMultiplier,
         ActualAtkOmni,
         ActualDefOmni
      ),

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
      shr_statistics:type(),
      shr_omnimods:type(),
      shr_statistics:type(),
      shr_omnimods:type()
   )
   -> maybe_type().
get_description_of ({first, CanParry}, AtkStats, AtkOmni, DefStats, DefOmni) ->
   effect_of_attack(first, AtkStats, AtkOmni, DefStats, DefOmni, CanParry);
get_description_of ({second, CanParry}, AtkStats, AtkOmni, DefStats, DefOmni) ->
   AttackerDoubleAttackChange =
      shr_statistics:get_double_hits(AtkStats),

   case shr_roll:percentage() of
      X when (X =< AttackerDoubleAttackChange) ->
         effect_of_attack
         (
            second,
            AtkStats,
            AtkOmni,
            DefStats,
            DefOmni,
            CanParry
         );

      _ ->
         nothing
   end;
get_description_of
(
   {counter, CanParry},
   AtkStats,
   AtkOmni,
   DefStats,
   DefOmni
) ->
   effect_of_attack(counter, DefStats, DefOmni, AtkStats, AtkOmni, CanParry).

-spec apply_to_healths
   (
      maybe_type(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> {maybe_type(), non_neg_integer(), non_neg_integer()}.
apply_to_healths
(
   nothing,
   AttackerHealth,
   DefenderHealth
) ->
   {nothing, AttackerHealth, DefenderHealth};
apply_to_healths
(
   _Attack,
   AttackerHealth,
   DefenderHealth
)
when
(
   (AttackerHealth =< 0)
   or (DefenderHealth =< 0)
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
      (DefenderHealth - Damage)
   };
apply_to_healths
(
   Attack,
   AttackerHealth,
   DefenderHealth
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
      DefenderHealth
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

-spec encode (type()) -> {list(any())}.
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
