-module(bm_attack).

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
      bm_statistics:type(),
      bm_statistics:type()
   )
   -> precision().
roll_precision (AttackerStatistics, DefenderStatistics) ->
   DefenderDodges = bm_statistics:get_dodges(DefenderStatistics),
   AttackerAccuracy = bm_statistics:get_accuracy(AttackerStatistics),
   MissChance = max(0, (DefenderDodges - AttackerAccuracy)),
   case sh_roll:percentage() of
      X when (X =< MissChance) -> misses;
      X when (X =< (MissChance * 2)) -> grazes;
      _ -> hits
   end.

-spec roll_damage
   (
      bm_statistics:type(),
      bm_statistics:type()
   )
   -> {non_neg_integer(), boolean()}.
roll_damage (AttackerStatistics, _DefenderStatistics) ->
   {MinimumDamage, MaximumDamage} =
      bm_statistics:get_damages(AttackerStatistics),
   MaximumRoll = max(1, MaximumDamage - MinimumDamage),
   BaseDamage = MinimumDamage + (rand:uniform(MaximumRoll) - 1),
   CriticalHitChance = bm_statistics:get_critical_hits(AttackerStatistics),
   case sh_roll:percentage() of
      X when (X =< CriticalHitChance) -> {(BaseDamage * 2), true};
      _ -> {BaseDamage, false}
   end.

-spec roll_parry (bm_statistics:type()) -> boolean().
roll_parry (DefenderStatistics) ->
   DefenderParryChance = bm_statistics:get_parries(DefenderStatistics),
   (sh_roll:percentage() =< DefenderParryChance).

-spec effect_of_attack
   (
      order(),
      bm_statistics:type(),
      bm_statistics:type(),
      boolean()
   )
   -> type().
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
      bm_statistics:type(),
      bm_statistics:type()
   )
   -> maybe_type().
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
   AttackerDoubleAttackChange =
      bm_statistics:get_double_hits(AttackerStatistics),

   case sh_roll:percentage() of
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
      bm_weapon:type(),
      bm_weapon:type()
   )
   -> list(step()).
get_sequence (AttackRange, AttackerWeapon, DefenderWeapon) ->
   {AttackerDefenseRange, AttackerAttackRange} =
      bm_weapon:get_ranges(AttackerWeapon),
   {DefenderDefenseRange, DefenderAttackRange} =
      bm_weapon:get_ranges(DefenderWeapon),

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
