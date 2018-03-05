-module(attack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: find better names for those types.
-type hits() :: ('misses' | 'grazes' | 'hits').
-type critical() :: ('critical' | 'basic').
-type attack_order() :: 'first' | 'second' | 'counter'.
-type attack_order_with_parry() :: {attack_order(), boolean()}.
-type attack_category() ::
   (
      attack_order()
      | {attack_order(), 'parry'}
   ).
-type attack_effect() :: {hits(), critical(), non_neg_integer()}.
-type attack_desc() ::
   (
      {attack_category(), attack_effect()}
      | 'nothing'
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type
(
   [
      hits/0,
      critical/0,
      attack_category/0,
      attack_effect/0,
      attack_desc/0,
      attack_order_with_parry/0
   ]
).

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
-spec roll_hits
   (
      statistics:struct(),
      statistics:struct()
   )
   -> hits().
roll_hits (AttackerStatistics, DefenderStatistics) ->
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
   -> {critical(), non_neg_integer()}.
roll_damage (AttackerStatistics, _DefenderStatistics) ->
   {MinimumDamage, MaximumDamage} = statistics:get_damages(AttackerStatistics),
   MaximumRoll = max(1, MaximumDamage - MinimumDamage),
   BaseDamage = MinimumDamage + (rand:uniform(MaximumRoll) - 1),
   CriticalHitChance = statistics:get_critical_hits(AttackerStatistics),
   case roll:percentage() of
      X when (X =< CriticalHitChance) -> {critical, (BaseDamage * 2)};
      _ -> {basic, BaseDamage}
   end.

-spec effect_of_attack
   (
      statistics:struct(),
      statistics:struct()
   )
   -> attack_effect().
effect_of_attack (AttackerStatistics, DefenderStatistics) ->
   Hits = roll_hits(AttackerStatistics, DefenderStatistics),
   {Critical, Damage} = roll_damage(AttackerStatistics, DefenderStatistics),
   case Hits of
      misses -> {Hits, Critical, 0};
      grazes -> {Hits, Critical, trunc(Damage / 2)};
      hits -> {Hits, Critical, Damage}
   end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_description_of
   (
      attack_order_with_parry(),
      statistics:struct(),
      statistics:struct()
   )
   -> attack_desc().
get_description_of
(
   {first, DefenderCanParry},
   AttackerStatistics,
   DefenderStatistics
) ->
   DefenderParryChance = statistics:get_parries(DefenderStatistics),
   ParryRoll =
      case DefenderCanParry of
         true -> roll:percentage();
         _ -> 101
      end,
   if
      (ParryRoll =< DefenderParryChance) ->
            {
               {first, parry},
               effect_of_attack(DefenderStatistics, AttackerStatistics)
            };

      true ->
         {first, effect_of_attack(AttackerStatistics, DefenderStatistics)}
   end;
get_description_of
(
   {second, DefenderCanParry},
   AttackerStatistics,
   DefenderStatistics
) ->
   DefenderParryChance = statistics:get_parries(DefenderStatistics),
   ParryRoll =
      case DefenderCanParry of
         true -> roll:percentage();
         _ -> 101
      end,
   AttackerDoubleAttackChange = statistics:get_double_hits(AttackerStatistics),
   DoubleAttackRoll = roll:percentage(),
   if
      (DoubleAttackRoll > AttackerDoubleAttackChange) ->
         nothing;

      (ParryRoll =< DefenderParryChance) ->
            {
               {second, parry},
               effect_of_attack(DefenderStatistics, AttackerStatistics)
            };

      true ->
         {second, effect_of_attack(AttackerStatistics, DefenderStatistics)}
   end;
get_description_of
(
   {counter, AttackerCanParry},
   AttackerStatistics,
   DefenderStatistics
) ->
   AttackerParryChance = statistics:get_parries(AttackerStatistics),
   ParryRoll =
      case AttackerCanParry of
         true -> roll:percentage();
         _ -> 101
      end,
   if
      (ParryRoll =< AttackerParryChance) ->
            {
               {counter, parry},
               effect_of_attack(AttackerStatistics, DefenderStatistics)
            };

      true ->
         {counter, effect_of_attack(DefenderStatistics, AttackerStatistics)}
   end.

-spec apply_to_healths
   (
      attack_desc(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> {attack_desc(), non_neg_integer(), non_neg_integer()}.
apply_to_healths
(
   nothing,
   AttackerHealth,
   DefenderHealth
) ->
   {nothing, AttackerHealth, DefenderHealth};
apply_to_healths
(
   {Attack, Effect},
   AttackerHealth,
   DefenderHealth
)
when
(
   (Attack == first)
   or (Attack == second)
   or (Attack == {counter, parry})
) ->
   {_Hits, _Critical, Damage} = Effect,
   case AttackerHealth of
      0 ->
         {nothing, AttackerHealth, DefenderHealth};

      _ ->
         {
            {Attack, Effect},
            AttackerHealth,
            max(0, (DefenderHealth - Damage))
         }
   end;
apply_to_healths
(
   {Attack, Effect},
   AttackerHealth,
   DefenderHealth
)
when
(
   (Attack == {first, parry})
   or (Attack == {second, parry})
   or (Attack == counter)
) ->
   {_Hits, _Critical, Damage} = Effect,
   case DefenderHealth of
      0 ->
         {nothing, AttackerHealth, DefenderHealth};

      _ ->
         {
            {Attack, Effect},
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
   -> list(attack_order_with_parry()).
get_sequence (AttackRange, AttackerWeapon, DefenderWeapon) ->
   {AttackerDefenseRange, AttackerAttackRange} =
      weapon:get_ranges(AttackerWeapon),
   {DefenderDefenseRange, DefenderAttackRange} =
      weapon:get_ranges(DefenderWeapon),

   AttackerCanAttack = (AttackRange =< AttackerAttackRange),
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
      (not AttackerCanAttack) ->
         [];

      (not DefenderCanDefend) ->
         [First, Second];

      true ->
         [First, Counter, Second]
   end.

-spec encode (attack_desc()) -> binary().
% This shouldn't be a possibility. Types in this module are a mess...
encode ({AttackCategory, AttackEffect}) ->
   jiffy:encode
   (
      {
         [
            <<"attack">>,
            list_to_binary
            (
               io_lib:format
               (
                  "~p",
                  [{AttackCategory, AttackEffect}]
               )
            )
         ]
      }
   ).
