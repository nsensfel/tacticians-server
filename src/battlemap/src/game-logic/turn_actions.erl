-module(turn_actions).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_attack_sequence
   (
      character_instance:type(),
      character_instance:type(),
      list(attack:step())
   )
   -> {list(attack:type()), non_neg_integer(), non_neg_integer()}.
handle_attack_sequence
(
   CharacterInstance,
   TargetCharacterInstance,
   AttackSequence
) ->
   Character = character_instance:get_character(CharacterInstance),
   TargetCharacter = character_instance:get_character(TargetCharacterInstance),
   CharacterStatistics = character:get_statistics(Character),
   TargetCharacterStatistics = character:get_statistics(TargetCharacter),

   AttackPlannedEffects =
      lists:map
      (
         fun (AttackStep) ->
            attack:get_description_of
            (
               AttackStep,
               CharacterStatistics,
               TargetCharacterStatistics
            )
         end,
         AttackSequence
      ),

   lists:foldl
   (
      fun
      (
         AttackEffectCandidate,
         {AttackValidEffects, AttackerHealth, DefenderHealth}
      ) ->
         {AttackResult, NewAttackerHealth, NewDefenderHealth} =
            attack:apply_to_healths
            (
               AttackEffectCandidate,
               AttackerHealth,
               DefenderHealth
            ),
         case AttackResult of
            nothing -> {AttackValidEffects, AttackerHealth, DefenderHealth};
            _ ->
               {
                  (AttackValidEffects ++ [AttackResult]),
                  NewAttackerHealth,
                  NewDefenderHealth
               }
         end
      end,
      {
         [],
         character_instance:get_current_health(CharacterInstance),
         character_instance:get_current_health(TargetCharacterInstance)
      },
      AttackPlannedEffects
   ).

-spec handle_switch_weapon
   (
      character_turn_update:type()
   )
   -> character_turn_update:type().
handle_switch_weapon (Update) ->
   Data = character_turn_update:get_data(Update),
   CharacterInstance = character_turn_data:get_character_instance(Data),
   CharacterInstanceIX = character_turn_data:get_character_instance_ix(Data),
   Character = character_instance:get_character(CharacterInstance),
   CharacterAttributes = character:get_attributes(Character),
   {PrimaryWeaponID, SecondaryWeaponID} = character:get_weapon_ids(Character),

   UpdatedWeaponIDs = {SecondaryWeaponID, PrimaryWeaponID},
   UpdatedCharacterStatistics =
      statistics:new(CharacterAttributes, UpdatedWeaponIDs),
   UpdatedCharacter =
      character:set_statistics
      (
         UpdatedCharacterStatistics,
         character:set_weapon_ids(UpdatedWeaponIDs, Character)
      ),
   UpdatedCharacterInstance =
      character_instance:set_character(UpdatedCharacter, CharacterInstance),

   % TODO: db update entries...
   % {character_instance, CharacterInstanceIX, wp0, SecondaryWeaponID},
   % {character_instance, CharacterInstanceIX, wp1, PrimaryWeaponID}
   S0Update =
      character_turn_update:add_to_timeline
      (
         turn_result:new_character_switched_weapons(CharacterInstanceIX),
         Update
      ),

   UpdatedData =
      character_turn_data:set_character_instance
      (
         UpdatedCharacterInstance,
         Data
      ),

   character_turn_update:set_data(UpdatedData, S0Update).

-spec handle_move
   (
      character_turn_update:type(),
      battle_action:type()
   )
   -> character_turn_update:type().
handle_move (Update, BattleAction) ->
   Character = character_instance:get_character(CharacterInstance),
   CharacterStatistics = character:get_statistics(Character),
   Battlemap = battle:get_battlemap(Battle),
   Path = BattleAction#move.path,
   CharacterMovementPoints =
      statistics:get_movement_points(CharacterStatistics),

   ForbiddenLocations =
      array:foldl
      (
         fun (IX, CharInst, Prev) ->
            IsAlive = character_instance:get_is_alive(CharInst),
            if
               (IX == CharacterInstanceIX) -> Prev;
               (not IsAlive) -> Prev;
               true -> [character_instance:get_location(CharInst)|Prev]
            end
         end,
         [],
         battle:get_character_instances(Battle)
      ),

   {NewLocation, Cost} =
      movement:cross
      (
         Battlemap,
         ForbiddenLocations,
         Path,
         character_instance:get_location(CharacterInstance)
      ),

   true = (Cost =< CharacterMovementPoints),

   UpdatedCharacterInstance =
      character_instance:set_location(NewLocation, CharacterInstance),

   {
      % TODO: hide that into database_diff structs.
      [{character_instance, CharacterInstanceIX, loc, NewLocation}],
      % TODO: hide that into turn_result structs.
      [turn_result:new_character_moved(CharacterInstanceIX, Path, NewLocation)],
      Battle,
      UpdatedCharacterInstance
   }.

-spec handle_attack
   (
      character_turn_update:type(),
      battle_action:type()
   )
   -> character_turn_update:type().
handle_attack (Update, BattleAction) ->
   Character = character_instance:get_character(CharacterInstance),
   TargetIX = BattleAction#attack.target_ix,
   TargetCharacterInstance = battle:get_character_instance(TargetIX, Battle),
   TargetCharacter = character_instance:get_character(TargetCharacterInstance),

   Range =
      location:dist
      (
         character_instance:get_location(CharacterInstance),
         character_instance:get_location(TargetCharacterInstance)
      ),

   {AttackingWeaponID, _} = character:get_weapon_ids(Character),
   {DefendingWeaponID, _} = character:get_weapon_ids(TargetCharacter),

   AttackingWeapon = weapon:from_id(AttackingWeaponID),
   DefendingWeapon = weapon:from_id(DefendingWeaponID),

   AttackSequence =
      attack:get_sequence(Range, AttackingWeapon, DefendingWeapon),

   {AttackEffects, RemainingAttackerHealth, RemainingDefenderHealth} =
      handle_attack_sequence
      (
         CharacterInstance,
         TargetCharacterInstance,
         AttackSequence
      ),

   UpdatedCharacterInstance =
      character_instance:set_current_health
      (
         RemainingAttackerHealth,
         CharacterInstance
      ),

   UpdatedBattle =
      battle:set_character_instance
      (
         TargetIX,
         character_instance:set_current_health
         (
            RemainingDefenderHealth,
            TargetCharacterInstance
         ),
         Battle
      ),
   {
      % TODO: hide that into database_diff structs.
      [], % TODO
      [
         turn_result:new_character_attacked
         (
            CharacterInstanceIX,
            TargetIX,
            AttackEffects
         )
      ],
      UpdatedBattle,
      UpdatedCharacterInstance
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
(
   character_turn_update:type(),
   battle_action:type()
)
-> character_turn_update:type().
handle (Update, BattleAction) ->
   case battle_action:get_type(BattleAction) of
      move -> handle_move(Update, BattleAction);
      switch_weapon -> handle_switch_weapon(Update);
      attack -> handle_attack(Update, BattleAction)
   end.
