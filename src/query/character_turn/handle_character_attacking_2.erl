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
      X when (X < MissChance) -> misses;
      X when (X < (MissChance * 2)) -> grazes;
      _ -> hits
   end.

roll_damage (AttackerStatistics, _DefenderStatistics) ->
   {MinimumDamage, MaximumDamage} = statistics:get_damages(AttackerStatistics),
   MaximumRoll = max(1, MaximumDamage - MinimumDamage),
   MinimumDamage + (rand:uniform(MaximumRoll) - 1).

handle_character_attacking (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   ControlledCharacterStatistics =
      character:get_statistics(ControlledCharacter),
   TargettedCharacterInstance =
      array:get
      (
         Input#input.target_id,
         battlemap_instance:get_characters(BattlemapInstance)
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
   {DefendingWeaponRange, _} = weapon:get_ranges(DefendingWeapon),

   true = (RequiredRange =< AttackingWeaponRange),

   {Rolls, HealthMod} =
      handle_attack
      (
         ControlledCharacterStatistics,
         TargettedCharacterStatistics
      ).
