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
      sh_statistics:type(),
      sh_statistics:type()
   )
   -> precision().
roll_precision (AttackerStatistics, DefenderStatistics) ->
   DefenderDodges = sh_statistics:get_dodges(DefenderStatistics),
   AttackerAccuracy = sh_statistics:get_accuracy(AttackerStatistics),
   MissChance = max(0, (DefenderDodges - AttackerAccuracy)),
   case sh_roll:percentage() of
      X when (X =< MissChance) -> misses;
      X when (X =< (MissChance * 2)) -> grazes;
      _ -> hits
   end.

-spec roll_damage
   (
      sh_statistics:type(),
      sh_statistics:type()
   )
   -> {non_neg_integer(), boolean()}.
roll_damage (AttackerStatistics, _DefenderStatistics) ->
   {MinimumDamage, MaximumDamage} =
      sh_statistics:get_damages(AttackerStatistics),
   MaximumRoll = max(1, MaximumDamage - MinimumDamage),
   BaseDamage = MinimumDamage + (rand:uniform(MaximumRoll) - 1),
   CriticalHitChance = sh_statistics:get_critical_hits(AttackerStatistics),
   case sh_roll:percentage() of
      X when (X =< CriticalHitChance) -> {(BaseDamage * 2), true};
      _ -> {BaseDamage, false}
   end.

-spec roll_parry (sh_statistics:type()) -> boolean().
roll_parry (DefenderStatistics) ->
   DefenderParryChance = sh_statistics:get_parries(DefenderStatistics),
   (sh_roll:percentage() =< DefenderParryChance).

-spec effect_of_attack
   (
      order(),
      bm_character:type(),
      bm_character:type(),
      boolean()
   )
   -> type().
effect_of_attack (Order, Attacker, Defender, CanParry) ->
   AttackerStatistics = bm_character:get_statistics(Attacker),
   DefenderStatistics = bm_character:get_statistics(Defender),

   ParryIsSuccessful = (CanParry and roll_parry(DefenderStatistics)),

   {ActualAtkStatistics, ActualDefStatistics} =
      case ParryIsSuccessful of
         true -> {DefenderStatistics, AttackerStatistics};
         false -> {AttackerStatistics, DefenderStatistics}
      end,
   {ActualAttacker, ActualDefender} =
      case ParryIsSuccessful of
         true -> {Defender, Attacker};
         false -> {Attacker, Defender}
      end,

   ActualDefArmor = sh_armor:from_id(bm_character:get_armor_id(ActualDefender)),
   {ActualAtkWeaponID, _} = bm_character:get_weapon_ids(ActualAttacker),
   ActualAtkWeaponDmgType =
      sh_weapon:get_damage_type(sh_weapon:from_id(ActualAtkWeaponID)),

   Precision = roll_precision(ActualAtkStatistics, ActualDefStatistics),
   {Damage, IsCritical} = roll_damage(ActualAtkStatistics, ActualDefStatistics),
   S0Damage =
      case Precision of
         misses -> 0;
         grazes -> trunc(Damage / 2);
         hits -> Damage
      end,
   ArmorResistance =
      sh_armor:get_resistance_to(ActualAtkWeaponDmgType, ActualDefArmor),
   ActualDamage = max(0, (S0Damage - ArmorResistance)),

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
      bm_character:type(),
      bm_character:type()
   )
   -> maybe_type().
get_description_of ({first, CanParry}, Attacker, Defender) ->
   effect_of_attack(first, Attacker, Defender, CanParry);
get_description_of ({second, CanParry}, Attacker, Defender) ->
   AttackerStatistics = bm_character:get_statistics(Attacker),
   AttackerDoubleAttackChange =
      sh_statistics:get_double_hits(AttackerStatistics),

   case sh_roll:percentage() of
      X when (X =< AttackerDoubleAttackChange) ->
         effect_of_attack (second, Attacker, Defender, CanParry);

      _ ->
         nothing
   end;
get_description_of ({counter, CanParry}, Attacker, Defender) ->
   effect_of_attack(counter, Defender, Attacker, CanParry).

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
   (Attack#attack.order == first)
   or (Attack#attack.order == second)
   or ((Attack#attack.order == counter) and Attack#attack.is_parry)
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
   (Attack#attack.order == counter)
   or
   (
      (Attack#attack.is_parry)
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
      sh_weapon:type(),
      sh_weapon:type()
   )
   -> list(step()).
get_sequence (AttackRange, AttackerWeapon, DefenderWeapon) ->
   {AttackerDefenseRange, AttackerAttackRange} =
      sh_weapon:get_ranges(AttackerWeapon),
   {DefenderDefenseRange, DefenderAttackRange} =
      sh_weapon:get_ranges(DefenderWeapon),

   AttackerCanAttack = (AttackRange =< AttackerAttackRange),
   AttackerCanAttack = true,
   AttackerCanDefend =
      (AttackerCanAttack and (AttackRange > AttackerDefenseRange)),
   AttackerCanParry =
      (AttackerCanDefend and sh_weapon:can_parry(AttackerWeapon)),

   DefenderCanAttack = (AttackRange =< DefenderAttackRange),
   DefenderCanDefend =
      (DefenderCanAttack and (AttackRange > DefenderDefenseRange)),
   DefenderCanParry =
      (DefenderCanDefend and sh_weapon:can_parry(DefenderWeapon)),

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
