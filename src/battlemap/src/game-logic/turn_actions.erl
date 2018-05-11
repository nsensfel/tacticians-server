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
   Character = character_turn_data:get_character(Data),
   CharacterIX = character_turn_data:get_character_ix(Data),
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

   % TODO: db update entries...
   % {character, CharacterIX, wp0, SecondaryWeaponID},
   % {character, CharacterIX, wp1, PrimaryWeaponID}

   UpdatedData = character_turn_data:set_character(UpdatedCharacter, Data),

   character_turn_update:add_to_timeline
   (
      turn_result:new_character_switched_weapons(CharacterIX),
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
   Character = character_turn_data:get_character(Data),
   CharacterIX = character_turn_data:get_character_ix(Data),
   Battle = character_turn_data:get_battle(Data),
   Battlemap = battle:get_battlemap(Battle),

   ForbiddenLocations =
      array:foldl
      (
         fun (IX, Char, Prev) ->
            IsAlive = character:get_is_alive(Char),
            if
               (IX == CharacterIX) -> Prev;
               (not IsAlive) -> Prev;
               true -> [character:get_location(Char)|Prev]
            end
         end,
         [],
         battle:get_characters(Battle)
      ),

   {NewLocation, Cost} =
      movement:cross
      (
         Battlemap,
         ForbiddenLocations,
         Path,
         character:get_location(Character)
      ),

   {Cost, NewLocation}.

-spec assert_character_can_move
   (
      character_turn_data:type(),
      non_neg_integer()
   )
   -> 'ok'.
assert_character_can_move (Data, Cost) ->
   Character = character_turn_data:get_character(Data),
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
   Character = character_turn_data:get_character(Data),
   CharacterIX = character_turn_data:get_character_ix(Data),

   UpdatedCharacter = character:set_location(NewLocation, Character),

   UpdatedData = character_turn_data:set_character(UpdatedCharacter, Data),

   S0Update =
      character_turn_update:add_to_timeline
      (
         turn_result:new_character_moved(CharacterIX, Path, NewLocation),
         Update
      ),

   %[{character, CharacterIX, loc, NewLocation}],

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
      character:type(),
      character:type(),
      list(attack:step())
   )
   -> {list(attack:type()), non_neg_integer(), non_neg_integer()}.
handle_attack_sequence
(
   Character,
   TargetCharacter,
   AttackSequence
) ->
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
         character:get_current_health(Character),
         character:get_current_health(TargetCharacter)
      },
      AttackPlannedEffects
   ).

-spec get_attack_sequence
   (
      character:type(),
      character:type()
   )
   -> list(attack:step()).
get_attack_sequence (Character, TargetCharacter) ->
   Range =
      location:dist
      (
         character:get_location(Character),
         character:get_location(TargetCharacter)
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
   Character = character_turn_data:get_character(Data),
   CharacterIX = character_turn_data:get_character_ix(Data),
   TargetIX = battle_action:get_target_ix(BattleAction),
   TargetCharacter = battle:get_character(TargetIX, Battle),

   AttackSequence = get_attack_sequence(Character, TargetCharacter),

   {AttackEffects, RemainingAttackerHealth, RemainingDefenderHealth} =
      handle_attack_sequence
      (
         Character,
         TargetCharacter,
         AttackSequence
      ),

   UpdatedCharacter =
      character:set_current_health
      (
         RemainingAttackerHealth,
         Character
      ),

   UpdatedBattle =
      battle:set_character
      (
         TargetIX,
         character:set_current_health
         (
            RemainingDefenderHealth,
            TargetCharacter
         ),
         Battle
      ),

   S0Data = character_turn_data:set_battle(UpdatedBattle, Data),
   S1Data =
      character_turn_data:set_character
      (
         UpdatedCharacter,
         S0Data
      ),

   S0Update =
      character_turn_update:add_to_timeline
      (
         turn_result:new_character_attacked
         (
            CharacterIX,
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
