roll_hits (AttackerStatistics, DefenderStatistics) ->
   MissChance =
      max
      (
         0,
         (
            statistics:get_dodges(DefenderStatistics)
            -
            statistics:get_accuracy(AttackerStatistics)
         )
      ),
   case roll:percentage() of
      X when (X =< MissChance) -> misses;
      X when (X =< (MissChance * 2)) -> grazes;
      _ -> hits
   end.

roll_damage (AttackerStatistics, _DefenderStatistics) ->
   {MinimumDamage, MaximumDamage} = statistics:get_damages(AttackerStatistics),
   MaximumRoll = max(1, MaximumDamage - MinimumDamage),
   BaseDamage = MinimumDamage + (rand:uniform(MaximumRoll) - 1),
   CriticalHitChance = statistics:get_critical_hits(AttackerStatistics),
   case roll:percentage() of
      X when (X =< CriticalHitChance) ->
         {critical, (BaseDamage * 2)};
      _ ->
         {basic, BaseDamage}
   end.

handle_attack (AttackerStatistics, DefenderStatistics) ->
   Hits = roll_hits(AttackerStatistics, DefenderStatistics),
   {Critical, Damage} = roll_damage(AttackerStatistics, DefenderStatistics),
   case Hits of
      misses ->
         {Hits, Critical, 0};

      grazes ->
         {
            Hits,
            Critical,
            trunc(Damage / 2)
         };

      _ ->
         {
            Hits,
            Critical,
            Damage
         }
   end.

handle_parry (AttackerStatistics, DefenderStatistics) ->
   ParryChance = statistics:get_parries(DefenderStatistics),
   case roll:percentage() of
      X when (X =< ParryChance) ->
         [{parry, handle_attack(DefenderStatistics, AttackerStatistics)}];
      _ ->
         []
   end.

handle_attacks ([], _AttackerStatistics, _DefenderStatistics, Results) ->
   Results;
handle_attacks
(
   [nothing|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      Results
   );
handle_attacks
(
   [first|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      [
         {
            first,
            handle_attack(AttackerStatistics, DefenderStatistics)
         }
         |
         Results
      ]
   );
handle_attacks
(
   [second|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      [
         {
            second,
            handle_attack(AttackerStatistics, DefenderStatistics)
         }
         |
         Results
      ]
   );
handle_attacks
(
   [parry|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      (
         handle_parry(AttackerStatistics, DefenderStatistics)
         ++
         Results
      )
   );
handle_attacks
(
   [counter|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      [
         {
            counter,
            handle_attack(DefenderStatistics, AttackerStatistics)
         }
         |
         Results
      ]
   ).

apply_attacks_to_healths ([], AttackerHealth, DefenderHealth, ValidEffects) ->
   {ValidEffects, AttackerHealth, DefenderHealth};
apply_attacks_to_healths
(
   [{first, Effect}|Next],
   AttackerHealth,
   DefenderHealth,
   ValidEffects
) ->
   {_Hit, _Critical, Damage} = Effect,
   case (AttackerHealth > 0) of
      true ->
         apply_attacks_to_healths
         (
            Next,
            AttackerHealth,
            max(0, (DefenderHealth - Damage)),
            [{first, Effect}|ValidEffects]
         );
      false ->
         ValidEffects
   end;
apply_attacks_to_healths
(
   [{second, Effect}|Next],
   AttackerHealth,
   DefenderHealth,
   ValidEffects
) ->
   {_Hit, _Critical, Damage} = Effect,
   case (AttackerHealth > 0) of
      true ->
         apply_attacks_to_healths
         (
            Next,
            AttackerHealth,
            max(0, (DefenderHealth - Damage)),
            [{second, Effect}|ValidEffects]
         );
      false ->
         ValidEffects
   end;
apply_attacks_to_healths
(
   [{counter, Effect}|Next],
   AttackerHealth,
   DefenderHealth,
   ValidEffects
) ->
   {_Hit, _Critical, Damage} = Effect,
   case (DefenderHealth > 0) of
      true ->
         apply_attacks_to_healths
         (
            Next,
            max(0, (AttackerHealth - Damage)),
            DefenderHealth,
            [{counter, Effect}|ValidEffects]
         );
      false ->
         ValidEffects
   end;
apply_attacks_to_healths
(
   [{parry, Effect}|Next],
   AttackerHealth,
   DefenderHealth,
   ValidEffects
) ->
   {_Hit, _Critical, Damage} = Effect,
   case (DefenderHealth > 0) of
      true ->
         apply_attacks_to_healths
         (
            Next,
            max(0, (AttackerHealth - Damage)),
            DefenderHealth,
            [{parry, Effect}|ValidEffects]
         );
      false ->
         ValidEffects
   end.

set_new_healths_in_query_state
(
   RemainingAttackerHealth,
   RemainingDefenderHealth,
   QueryState,
   Input
) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   CharacterInstances =
      battlemap_instance:get_character_instances(BattlemapInstance),
   TargettedCharacterInstanceIX = Input#input.target_ix,
   TargettedCharacterInstance =
      array:get
      (
         TargettedCharacterInstanceIX,
         CharacterInstances
      ),

   QueryState#query_state
   {
      battlemap_instance =
         battlemap_instance:set_character_instances
         (
            array:set
            (
               TargettedCharacterInstanceIX,
               character_instance:set_current_health
               (
                  TargettedCharacterInstance,
                  RemainingDefenderHealth
               )
            )
         ),
      character_instance =
         character_instance:set_current_health
         (
            ControlledCharacterInstance,
            RemainingAttackerHealth
         )
   }.

handle_character_instance_attacking (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   ControlledCharacterStatistics =
      character:get_statistics(ControlledCharacter),
   TargettedCharacterInstance =
      array:get
      (
         Input#input.target_ix,
         battlemap_instance:get_character_instances(BattlemapInstance)
      ),
   TargettedCharacter =
      character_instance:get_character(TargettedCharacterInstance),
   TargettedCharacterStatistics = character:get_statistics(TargettedCharacter),
   RequiredRange =
      movement:steps_between
      (
         character_instance:get_location(ControlledCharacterInstance),
         character_instance:get_location(TargettedCharacterInstance)
      ),
   {AttackingWeaponID, _} = character:get_weapon_ids(ControlledCharacter),
   AttackingWeapon = weapon:from_id(AttackingWeaponID),
   {_, AttackingWeaponRange} = weapon:get_ranges(AttackingWeapon),
   {DefendingWeaponID, _} = character:get_weapon_ids(TargettedCharacter),
   DefendingWeapon = weapon:from_id(DefendingWeaponID),
   {DefendingWeaponDefRange, DefendingWeaponAtkRange} =
      weapon:get_ranges(DefendingWeapon),

   true = (RequiredRange =< AttackingWeaponRange),

   CanDefend =
      (
         (RequiredRange > DefendingWeaponDefRange)
         and
         (RequiredRange =< DefendingWeaponAtkRange)
      ),
   CanParry = (weapon:get_range_type(DefendingWeapon) == melee),
   Actions =
      case {CanDefend, CanParry} of
         {true, true} ->
            [second, counter, parry, first];
         {true, false} ->
            [second, counter, first];
         {false, _} ->
            [second, first]
      end,
   Effects =
      handle_attacks
      (
         Actions,
         ControlledCharacterStatistics,
         TargettedCharacterStatistics,
         []
      ),
   {RemainingEffects, RemainingAttackerHealth, RemainingDefenderHealth} =
      apply_attacks_to_healths
      (
         Effects,
         character_instance:get_current_health(ControlledCharacterInstance),
         character_instance:get_current_health(TargettedCharacterInstance),
         []
      ),

   {
      RemainingEffects,
      set_new_healths_in_query_state
      (
         QueryState,
         RemainingAttackerHealth,
         RemainingDefenderHealth,
         Input
      )
   }.
