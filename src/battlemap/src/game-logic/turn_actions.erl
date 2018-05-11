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

%%%% SWITCHING WEAPON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

   UpdatedData =
      character_turn_data:set_character_instance
      (
         UpdatedCharacterInstance,
         Data
      ),

   character_turn_update:add_to_timeline
   (
      turn_result:new_character_switched_weapons(CharacterInstanceIX),
      character_turn_update:set_data(UpdatedData, Update)
   ).

%%%% MOVING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_path_cost_and_destination
   (
      character_turn_data:type(),
      list(direction:type())
   )
   -> {non_neg_integer(), location:type()}.
get_path_cost_and_destination (Data, Path) ->
   CharacterInstance = character_turn_data:get_character_instance(Data),
   CharacterInstanceIX = character_turn_data:get_character_instance_ix(Data),
   Battle = character_turn_data:get_battle(Data),
   Battlemap = battle:get_battlemap(Battle),

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

   {Cost, NewLocation}.

-spec assert_character_can_move
   (
      character_turn_data:type(),
      non_neg_integer()
   )
   -> 'ok'.
assert_character_can_move (Data, Cost) ->
   CharacterInstance = character_turn_data:get_character_instance(Data),
   Character = character_instance:get_character(CharacterInstance),
   CharacterStatistics = character:get_statistics(Character),
   CharacterMovementPoints =
      statistics:get_movement_points(CharacterStatistics),

   true = (Cost =< CharacterMovementPoints),

   ok.

-spec commit_move
   (
      character_turn_update:type(),
      list(direction:type()),
      location:type()
   )
   -> character_turn_update:type().
commit_move (Update, Path, NewLocation) ->
   Data = character_turn_update:get_data(Update),
   CharacterInstance = character_turn_data:get_character_instance(Data),
   CharacterInstanceIX = character_turn_data:get_character_instance_ix(Data),

   UpdatedCharacterInstance =
      character_instance:set_location(NewLocation, CharacterInstance),

   UpdatedData =
      character_turn_data:set_character_instance
      (
         UpdatedCharacterInstance,
         Data
      ),

   S0Update =
      character_turn_update:add_to_timeline
      (
         turn_result:new_character_moved
         (
            CharacterInstanceIX,
            Path,
            NewLocation
         ),
         Update
      ),

   %[{character_instance, CharacterInstanceIX, loc, NewLocation}],

   character_turn_update:set_data(UpdatedData, S0Update).

-spec handle_move
   (
      character_turn_update:type(),
      battle_action:type()
   )
   -> character_turn_update:type().
handle_move (Update, BattleAction) ->
   Data = character_turn_update:get_data(Update),
   Path = battle_action:get_path(BattleAction),

   {PathCost, NewLocation} = get_path_cost_and_destination(Data, Path),
   assert_character_can_move(Data, PathCost),

   commit_move(Update, Path, NewLocation).

%%%% ATTACKING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

-spec get_attack_sequence
   (
      character_instance:type(),
      character_instance:type()
   )
   -> list(attack:step()).
get_attack_sequence (CharacterInstance, TargetCharacterInstance) ->
   Character = character_instance:get_character(CharacterInstance),
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

   attack:get_sequence(Range, AttackingWeapon, DefendingWeapon).


-spec handle_attack
   (
      character_turn_update:type(),
      battle_action:type()
   )
   -> character_turn_update:type().
handle_attack (Update, BattleAction) ->
   Data = character_turn_update:get_data(Update),
   Battle = character_turn_data:get_battle(Data),
   CharacterInstance = character_turn_data:get_character_instance(Data),
   CharacterInstanceIX = character_turn_data:get_character_instance_ix(Data),
   TargetIX = battle_action:get_target_ix(BattleAction),
   TargetCharacterInstance = battle:get_character_instance(TargetIX, Battle),

   AttackSequence =
      get_attack_sequence(CharacterInstance, TargetCharacterInstance),

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

   S0Data = character_turn_data:set_battle(UpdatedBattle, Data),
   S1Data =
      character_turn_data:set_character_instance
      (
         UpdatedCharacterInstance,
         S0Data
      ),

   S0Update =
      character_turn_update:add_to_timeline
      (
         turn_result:new_character_attacked
         (
            CharacterInstanceIX,
            TargetIX,
            AttackEffects
         ),
         Update
      ),
   character_turn_update:set_data(S1Data, S0Update).

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
   case battle_action:get_category(BattleAction) of
      move -> handle_move(Update, BattleAction);
      switch_weapon -> handle_switch_weapon(Update);
      attack -> handle_attack(Update, BattleAction)
   end.
