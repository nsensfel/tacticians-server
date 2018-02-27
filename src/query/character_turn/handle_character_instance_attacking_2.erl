%% FIXME: parry not working as intended
roll_hits (AttackerStatistics, DefenderStatistics) ->
   DefenderDodges = statistics:get_dodges(DefenderStatistics),
   AttackerAccuracy = statistics:get_accuracy(AttackerStatistics),
   MissChance = max(0, (DefenderDodges - AttackerAccuracy)),
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
      X when (X =< CriticalHitChance) -> {critical, (BaseDamage * 2)};
      _ -> {basic, BaseDamage}
   end.

handle_attack (AttackerStatistics, DefenderStatistics) ->
   Hits = roll_hits(AttackerStatistics, DefenderStatistics),
   {Critical, Damage} = roll_damage(AttackerStatistics, DefenderStatistics),
   case Hits of
      misses -> {Hits, Critical, 0};
      grazes -> {Hits, Critical, trunc(Damage / 2)};
      hits -> {Hits, Critical, Damage}
   end.


handle_attacks ([], _AttackerStatistics, _DefenderStatistics, Results) ->
   Results;
handle_attacks
(
   [first|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   AttackResult = handle_attack(AttackerStatistics, DefenderStatistics),
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      [{first, AttackResult} | Results]
   );
handle_attacks
(
   [second|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   SecondHitChance = statistics:get_double_hits(AttackerStatistics),
   UpdatedResults =
      case roll:percentage() of
         X when (X =< SecondHitChance) ->
            [
               {second, handle_attack(AttackerStatistics, DefenderStatistics)}
               |
               Results
            ];

         _ ->
            Results
      end,
   handle_attacks(Next, AttackerStatistics, DefenderStatistics, UpdatedResults);
handle_attacks
(
   [{first, parry}|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   ParryChance = statistics:get_parries(DefenderStatistics),
   AttackResult =
      case roll:percentage() of
         X when (X =< ParryChance) ->
            {
               {first, parry},
               handle_attack(DefenderStatistics, AttackerStatistics)
            };

         _ ->
            {first, handle_attack(AttackerStatistics, DefenderStatistics)}
      end,
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      [AttackResult|Results]
   );
handle_attacks
(
   [{second, parry}|Next],
   AttackerStatistics,
   DefenderStatistics,
   Results
) ->
   SecondHitChance = statistics:get_double_hits(AttackerStatistics),
   ParryChance = statistics:get_parries(DefenderStatistics),
   AttackResult =
      case roll:percentage() of
         X when (X =< SecondHitChance) ->
            case roll:percentage() of
               Y when (Y =< ParryChance) ->
                  {
                     {second, parry},
                     handle_attack(DefenderStatistics, AttackerStatistics)
                  };

               _ ->
                  {
                     second,
                     handle_attack(AttackerStatistics, DefenderStatistics)
                  }
            end;

         _ -> nothing
      end,
   handle_attacks
   (
      Next,
      AttackerStatistics,
      DefenderStatistics,
      case AttackResult of
         nothing -> Results;
         _ -> [AttackResult|Results]
      end
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
         {counter, handle_attack(DefenderStatistics, AttackerStatistics)}
         |
         Results
      ]
   ).

apply_attacks_to_healths ([], AttackerHealth, DefenderHealth, ValidEffects) ->
   {ValidEffects, AttackerHealth, DefenderHealth};
apply_attacks_to_healths
(
   [{Action, Effect}|Next],
   AttackerHealth,
   DefenderHealth,
   ValidEffects
)
when ((Action == first) or (Action == second)) ->
   {_Hit, _Critical, Damage} = Effect,
   case (AttackerHealth > 0) of
      true ->
         apply_attacks_to_healths
         (
            Next,
            AttackerHealth,
            max(0, (DefenderHealth - Damage)),
            [{Action, Effect}|ValidEffects]
         );

      false ->
         {ValidEffects, AttackerHealth, DefenderHealth}
   end;
apply_attacks_to_healths
(
   [{Action, Effect}|Next],
   AttackerHealth,
   DefenderHealth,
   ValidEffects
)
when
(
   (Action == counter)
   or (Action == {first, parry})
   or (Action == {second, parry})
) ->
   {_Hit, _Critical, Damage} = Effect,
   case (DefenderHealth > 0) of
      true ->
         apply_attacks_to_healths
         (
            Next,
            max(0, (AttackerHealth - Damage)),
            DefenderHealth,
            [{Action, Effect}|ValidEffects]
         );

      false ->
         {ValidEffects, AttackerHealth, DefenderHealth}
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
                  RemainingDefenderHealth,
                  TargettedCharacterInstance
               ),
               CharacterInstances
            ),
            BattlemapInstance
         ),
      character_instance =
         character_instance:set_current_health
         (
            RemainingAttackerHealth,
            ControlledCharacterInstance
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
            [{second, parry}, counter, {first, parry}];
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
         RemainingAttackerHealth,
         RemainingDefenderHealth,
         QueryState,
         Input
      )
   }.