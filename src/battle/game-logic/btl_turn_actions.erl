-module(btl_turn_actions).
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
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_switch_weapon (Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),
   CharacterAttributes = btl_character:get_attributes(Character),
   ArmorID = btl_character:get_armor_id(Character),
   {PrimaryWeaponID, SecondaryWeaponID} = btl_character:get_weapon_ids(Character),

   UpdatedWeaponIDs = {SecondaryWeaponID, PrimaryWeaponID},
   UpdatedCharacterStatistics =
      shr_statistics:new(CharacterAttributes, UpdatedWeaponIDs, ArmorID),
   UpdatedCharacter =
      btl_character:set_statistics
      (
         UpdatedCharacterStatistics,
         btl_character:set_weapon_ids(UpdatedWeaponIDs, Character)
      ),

   TimelineItem = btl_turn_result:new_character_switched_weapons(CharacterIX),

   DBQuery =
      shr_db_query:update_indexed
      (
         btl_battle:get_characters_field(),
         CharacterIX,
         [
            shr_db_query:set_field
            (
               btl_character:get_weapons_field(),
               UpdatedWeaponIDs
            ),
            shr_db_query:set_field
            (
               btl_character:get_statistics_field(),
               UpdatedCharacterStatistics
            )
         ]
      ),

   UpdatedData = btl_character_turn_data:set_character(UpdatedCharacter, Data),

   S0Update = btl_character_turn_update:set_data(UpdatedData, Update),

   btl_character_turn_update:add_to_timeline(TimelineItem, DBQuery, S0Update).

%%%% MOVING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_path_cost_and_destination
   (
      btl_character_turn_data:type(),
      list(btl_direction:type())
   )
   -> {non_neg_integer(), btl_location:type()}.
get_path_cost_and_destination (Data, Path) ->
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),
   Battle = btl_character_turn_data:get_battle(Data),
   Battlemap = btl_battle:get_map(Battle),

   ForbiddenLocations =
      array:foldl
      (
         fun (IX, Char, Prev) ->
            IsAlive = btl_character:get_is_alive(Char),
            if
               (IX == CharacterIX) -> Prev;
               (not IsAlive) -> Prev;
               true -> [btl_character:get_location(Char)|Prev]
            end
         end,
         [],
         btl_battle:get_characters(Battle)
      ),

   {NewLocation, Cost} =
      btl_movement:cross
      (
         Battlemap,
         ForbiddenLocations,
         Path,
         btl_character:get_location(Character)
      ),

   {Cost, NewLocation}.

-spec assert_character_can_move
   (
      btl_character_turn_data:type(),
      non_neg_integer()
   )
   -> 'ok'.
assert_character_can_move (Data, Cost) ->
   Character = btl_character_turn_data:get_character(Data),
   CharacterStatistics = btl_character:get_statistics(Character),
   CharacterMovementPoints =
      shr_statistics:get_movement_points(CharacterStatistics),

   true = (Cost =< CharacterMovementPoints),

   ok.

-spec commit_move
   (
      btl_character_turn_update:type(),
      list(btl_direction:type()),
      btl_location:type()
   )
   -> btl_character_turn_update:type().
commit_move (Update, Path, NewLocation) ->
   Data = btl_character_turn_update:get_data(Update),
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),

   UpdatedCharacter = btl_character:set_location(NewLocation, Character),

   UpdatedData = btl_character_turn_data:set_character(UpdatedCharacter, Data),

   TimelineItem =
      btl_turn_result:new_character_moved(CharacterIX, Path, NewLocation),

   DBQuery =
      shr_db_query:update_indexed
      (
         btl_battle:get_characters_field(),
         CharacterIX,
         [
            shr_db_query:set_field
            (
               btl_character:get_location_field(),
               NewLocation
            )
         ]
      ),

   S0Update =
      btl_character_turn_update:add_to_timeline
      (
         TimelineItem,
         DBQuery,
         Update
      ),

   btl_character_turn_update:set_data(UpdatedData, S0Update).

-spec handle_move
   (
      btl_battle_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_move (BattleAction, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Path = btl_battle_action:get_path(BattleAction),

   {PathCost, NewLocation} = get_path_cost_and_destination(Data, Path),
   assert_character_can_move(Data, PathCost),

   commit_move(Update, Path, NewLocation).

%%%% ATTACKING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_attack_sequence
   (
      btl_character:type(),
      btl_character:type(),
      list(btl_attack:step())
   )
   -> {list(btl_attack:type()), non_neg_integer(), non_neg_integer()}.
handle_attack_sequence
(
   Character,
   TargetCharacter,
   AttackSequence
) ->
   AttackPlannedEffects =
      lists:map
      (
         fun (AttackStep) ->
            btl_attack:get_description_of
            (
               AttackStep,
               Character,
               TargetCharacter
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
            btl_attack:apply_to_healths
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
         btl_character:get_current_health(Character),
         btl_character:get_current_health(TargetCharacter)
      },
      AttackPlannedEffects
   ).

-spec get_attack_sequence
   (
      btl_character:type(),
      btl_character:type()
   )
   -> list(btl_attack:step()).
get_attack_sequence (Character, TargetCharacter) ->
   Range =
      btl_location:dist
      (
         btl_character:get_location(Character),
         btl_character:get_location(TargetCharacter)
      ),

   {AttackingWeaponID, _} = btl_character:get_weapon_ids(Character),
   {DefendingWeaponID, _} = btl_character:get_weapon_ids(TargetCharacter),

   AttackingWeapon = shr_weapon:from_id(AttackingWeaponID),
   DefendingWeapon = shr_weapon:from_id(DefendingWeaponID),

   btl_attack:get_sequence(Range, AttackingWeapon, DefendingWeapon).


-spec handle_attack
   (
      btl_battle_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_attack (BattleAction, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),
   TargetIX = btl_battle_action:get_target_ix(BattleAction),
   TargetCharacter = btl_battle:get_character(TargetIX, Battle),

   true = btl_character:get_is_alive(TargetCharacter),

   AttackSequence = get_attack_sequence(Character, TargetCharacter),

   {AttackEffects, RemainingAttackerHealth, RemainingDefenderHealth} =
      handle_attack_sequence(Character, TargetCharacter, AttackSequence),

   UpdatedCharacter =
      btl_character:set_current_health(RemainingAttackerHealth, Character),

   UpdatedBattle =
      btl_battle:set_character
      (
         TargetIX,
         btl_character:set_current_health
         (
            RemainingDefenderHealth,
            TargetCharacter
         ),
         Battle
      ),

   S0Data = btl_character_turn_data:set_battle(UpdatedBattle, Data),
   S1Data = btl_character_turn_data:set_character(UpdatedCharacter, S0Data),

   TimelineItem =
      btl_turn_result:new_character_attacked
      (
         CharacterIX,
         TargetIX,
         AttackEffects
      ),

   DBQuery0 =
      shr_db_query:update_indexed
      (
         btl_battle:get_characters_field(),
         TargetIX,
         [
            shr_db_query:set_field
            (
               btl_character:get_current_health_field(),
               RemainingDefenderHealth
            )
         ]
      ),

   DBQuery1 =
      shr_db_query:update_indexed
      (
         btl_battle:get_characters_field(),
         CharacterIX,
         [
            shr_db_query:set_field
            (
               btl_character:get_current_health_field(),
               RemainingAttackerHealth
            )
         ]
      ),

   S0Update =
      btl_character_turn_update:add_to_timeline
      (
         TimelineItem,
         DBQuery0,
         Update
      ),

   S1Update =
      btl_character_turn_update:add_to_db
      (
         DBQuery1,
         S0Update
      ),

   S2Update = btl_character_turn_update:set_data(S1Data, S1Update),

   S3Update =
      btl_victory:handle_character_lost_health
      (
         CharacterIX,
         RemainingAttackerHealth,
         S2Update
      ),

   S4Update =
      btl_victory:handle_character_lost_health
      (
         TargetIX,
         RemainingDefenderHealth,
         S3Update
      ),

   S4Update.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
(
   btl_battle_action:type(),
   btl_character_turn_update:type()
)
-> btl_character_turn_update:type().
handle (BattleAction, Update) ->
   case btl_battle_action:get_category(BattleAction) of
      move -> handle_move(BattleAction, Update);
      switch_weapon -> handle_switch_weapon(Update);
      attack -> handle_attack(BattleAction, Update)
   end.
