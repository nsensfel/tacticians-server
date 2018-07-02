-module(bm_turn_actions).
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
-spec handle_player_defeat
   (
      non_neg_integer(),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
handle_player_defeat (PlayerIX, Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   Characters = bm_battle:get_characters(Battle),

   %% FIXME: The controlled character might slip through.
   {UpdatedCharacters, ModifiedIXs} =
      sh_array_util:mapiff
      (
         fun (Character) ->
            (bm_character:get_player_index(Character) == PlayerIX)
         end,
         fun (Character) ->
            bm_character:set_rank(defeated, Character)
         end,
         Characters
      ),

   S1Update =
      lists:foldl
      (
         fun (NextUpdate, IX) ->
            bm_character_turn_update:add_to_db
            (
               sh_db_query:update_indexed
               (
                  bm_battle:get_characters_field(),
                  IX,
                  [
                     sh_db_query:set_field
                     (
                        bm_character:get_rank_field(),
                        defeated
                     )
                  ]
               ),
               NextUpdate
            )
         end,
         Update,
         ModifiedIXs
      ),

   %% TODO: Battle.player[PlayerIX].is_active <- false

   UpdatedBattle = bm_battle:set_characters(UpdatedCharacters, Battle),
   UpdatedData = bm_character_turn_data:set_battle(UpdatedBattle, Data),
   S2Update = bm_character_turn_update:set_data(UpdatedData, S1Update),

   DBQuery =
      sh_db_query:update_indexed
      (
         bm_battle:get_players_field(),
         PlayerIX,
         [
            sh_db_query:set_field
            (
               bm_character:get_is_active_field(),
               false
            )
         ]
      ),

   S3Update =
      bm_character_turn_update:add_to_timeline
      (
         bm_turn_result:new_player_lost(PlayerIX),
         DBQuery,
         S2Update
      ),

   S3Update.

-spec handle_victory_condition
   (
      non_neg_integer(),
      integer(),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
handle_victory_condition (_, Health, Update) when (Health > 0) -> Update;
handle_victory_condition (CharIX, _Health, Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   Character = bm_battle:get_character(CharIX, Battle),
   Characters = bm_battle:get_characters(Battle),
   CharacterPlayerIX = bm_character:get_player_index(Character),

   case bm_character:get_rank(Character) of
      optional ->
         %% Let's not assume there is a commander
         StillHasAliveChar =
            sh_array_util:any_indexed
            (
               fun (IX, Char) ->
                  (
                     (CharacterPlayerIX == bm_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and bm_character:get_is_alive(Char)
                  )
               end,
               %% FIXME: Potential issue if it's the controlled player and Data
               %% is dirty.
               Characters
            ),

         case StillHasAliveChar of
            true -> Update;
            _ -> handle_player_defeat(CharacterPlayerIX, Update)
         end;

      commander -> handle_player_defeat(CharacterPlayerIX, Update);

      target ->
         StillHasAliveChar =
            sh_array_util:any_indexed
            (
               fun (IX, Char) ->
                  (
                     (CharacterPlayerIX == bm_character:get_player_index(Char))
                     and (IX /= CharIX)
                     and bm_character:get_is_alive(Char)
                     and (bm_character:get_rank(Char) == target)
                  )
               end,
               %% FIXME: Potential issue if it's the controlled player and Data
               %% is dirty.
               Characters
            ),

         case StillHasAliveChar of
            true -> Update;
            _ -> handle_player_defeat(CharacterPlayerIX, Update)
         end;

      defeated -> Update
   end.

%%%% SWITCHING WEAPON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_switch_weapon
   (
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
handle_switch_weapon (Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Character = bm_character_turn_data:get_character(Data),
   CharacterIX = bm_character_turn_data:get_character_ix(Data),
   CharacterAttributes = bm_character:get_attributes(Character),
   ArmorID = bm_character:get_armor_id(Character),
   {PrimaryWeaponID, SecondaryWeaponID} = bm_character:get_weapon_ids(Character),

   UpdatedWeaponIDs = {SecondaryWeaponID, PrimaryWeaponID},
   UpdatedCharacterStatistics =
      sh_statistics:new(CharacterAttributes, UpdatedWeaponIDs, ArmorID),
   UpdatedCharacter =
      bm_character:set_statistics
      (
         UpdatedCharacterStatistics,
         bm_character:set_weapon_ids(UpdatedWeaponIDs, Character)
      ),

   TimelineItem = bm_turn_result:new_character_switched_weapons(CharacterIX),

   DBQuery =
      sh_db_query:update_indexed
      (
         bm_battle:get_characters_field(),
         CharacterIX,
         [
            sh_db_query:set_field
            (
               bm_character:get_weapons_field(),
               UpdatedWeaponIDs
            ),
            sh_db_query:set_field
            (
               bm_character:get_statistics_field(),
               UpdatedCharacterStatistics
            )
         ]
      ),

   UpdatedData = bm_character_turn_data:set_character(UpdatedCharacter, Data),

   S0Update = bm_character_turn_update:set_data(UpdatedData, Update),

   bm_character_turn_update:add_to_timeline(TimelineItem, DBQuery, S0Update).

%%%% MOVING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_path_cost_and_destination
   (
      bm_character_turn_data:type(),
      list(bm_direction:type())
   )
   -> {non_neg_integer(), bm_location:type()}.
get_path_cost_and_destination (Data, Path) ->
   Character = bm_character_turn_data:get_character(Data),
   CharacterIX = bm_character_turn_data:get_character_ix(Data),
   Battle = bm_character_turn_data:get_battle(Data),
   Battlemap = bm_battle:get_battlemap(Battle),

   ForbiddenLocations =
      array:foldl
      (
         fun (IX, Char, Prev) ->
            IsAlive = bm_character:get_is_alive(Char),
            if
               (IX == CharacterIX) -> Prev;
               (not IsAlive) -> Prev;
               true -> [bm_character:get_location(Char)|Prev]
            end
         end,
         [],
         bm_battle:get_characters(Battle)
      ),

   {NewLocation, Cost} =
      bm_movement:cross
      (
         Battlemap,
         ForbiddenLocations,
         Path,
         bm_character:get_location(Character)
      ),

   {Cost, NewLocation}.

-spec assert_character_can_move
   (
      bm_character_turn_data:type(),
      non_neg_integer()
   )
   -> 'ok'.
assert_character_can_move (Data, Cost) ->
   Character = bm_character_turn_data:get_character(Data),
   CharacterStatistics = bm_character:get_statistics(Character),
   CharacterMovementPoints =
      sh_statistics:get_movement_points(CharacterStatistics),

   true = (Cost =< CharacterMovementPoints),

   ok.

-spec commit_move
   (
      bm_character_turn_update:type(),
      list(bm_direction:type()),
      bm_location:type()
   )
   -> bm_character_turn_update:type().
commit_move (Update, Path, NewLocation) ->
   Data = bm_character_turn_update:get_data(Update),
   Character = bm_character_turn_data:get_character(Data),
   CharacterIX = bm_character_turn_data:get_character_ix(Data),

   UpdatedCharacter = bm_character:set_location(NewLocation, Character),

   UpdatedData = bm_character_turn_data:set_character(UpdatedCharacter, Data),

   TimelineItem =
      bm_turn_result:new_character_moved(CharacterIX, Path, NewLocation),

   DBQuery =
      sh_db_query:update_indexed
      (
         bm_battle:get_characters_field(),
         CharacterIX,
         [
            sh_db_query:set_field
            (
               bm_character:get_location_field(),
               NewLocation
            )
         ]
      ),

   S0Update =
      bm_character_turn_update:add_to_timeline
      (
         TimelineItem,
         DBQuery,
         Update
      ),

   bm_character_turn_update:set_data(UpdatedData, S0Update).

-spec handle_move
   (
      bm_battle_action:type(),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
handle_move (BattleAction, Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Path = bm_battle_action:get_path(BattleAction),

   {PathCost, NewLocation} = get_path_cost_and_destination(Data, Path),
   assert_character_can_move(Data, PathCost),

   commit_move(Update, Path, NewLocation).

%%%% ATTACKING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_attack_sequence
   (
      bm_character:type(),
      bm_character:type(),
      list(bm_attack:step())
   )
   -> {list(bm_attack:type()), non_neg_integer(), non_neg_integer()}.
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
            bm_attack:get_description_of
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
            bm_attack:apply_to_healths
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
         bm_character:get_current_health(Character),
         bm_character:get_current_health(TargetCharacter)
      },
      AttackPlannedEffects
   ).

-spec get_attack_sequence
   (
      bm_character:type(),
      bm_character:type()
   )
   -> list(bm_attack:step()).
get_attack_sequence (Character, TargetCharacter) ->
   Range =
      bm_location:dist
      (
         bm_character:get_location(Character),
         bm_character:get_location(TargetCharacter)
      ),

   {AttackingWeaponID, _} = bm_character:get_weapon_ids(Character),
   {DefendingWeaponID, _} = bm_character:get_weapon_ids(TargetCharacter),

   AttackingWeapon = sh_weapon:from_id(AttackingWeaponID),
   DefendingWeapon = sh_weapon:from_id(DefendingWeaponID),

   bm_attack:get_sequence(Range, AttackingWeapon, DefendingWeapon).


-spec handle_attack
   (
      bm_battle_action:type(),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
handle_attack (BattleAction, Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   Character = bm_character_turn_data:get_character(Data),
   CharacterIX = bm_character_turn_data:get_character_ix(Data),
   TargetIX = bm_battle_action:get_target_ix(BattleAction),
   TargetCharacter = bm_battle:get_character(TargetIX, Battle),

   AttackSequence = get_attack_sequence(Character, TargetCharacter),

   {AttackEffects, RemainingAttackerHealth, RemainingDefenderHealth} =
      handle_attack_sequence(Character, TargetCharacter, AttackSequence),

   UpdatedCharacter =
      bm_character:set_current_health(RemainingAttackerHealth, Character),

   UpdatedBattle =
      bm_battle:set_character
      (
         TargetIX,
         bm_character:set_current_health
         (
            RemainingDefenderHealth,
            TargetCharacter
         ),
         Battle
      ),


   S0Data = bm_character_turn_data:set_battle(UpdatedBattle, Data),
   S1Data = bm_character_turn_data:set_character(UpdatedCharacter, S0Data),

   TimelineItem =
      bm_turn_result:new_character_attacked
      (
         CharacterIX,
         TargetIX,
         AttackEffects
      ),

   DBQuery0 =
      sh_db_query:update_indexed
      (
         bm_battle:get_characters_field(),
         TargetIX,
         [
            sh_db_query:set_field
            (
               bm_character:get_current_health_field(),
               RemainingDefenderHealth
            )
         ]
      ),

   DBQuery1 =
      sh_db_query:update_indexed
      (
         bm_battle:get_characters_field(),
         CharacterIX,
         [
            sh_db_query:set_field
            (
               bm_character:get_current_health_field(),
               RemainingAttackerHealth
            )
         ]
      ),

   S0Update =
      bm_character_turn_update:add_to_timeline
      (
         TimelineItem,
         DBQuery0,
         Update
      ),

   S1Update =
      bm_character_turn_update:add_to_db
      (
         DBQuery1,
         S0Update
      ),

   S2Update = bm_character_turn_update:set_data(S1Data, S1Update),

   S3Update =
      handle_victory_condition
      (
         CharacterIX,
         RemainingAttackerHealth,
         S2Update
      ),

   S4Update =
      handle_victory_condition
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
   bm_battle_action:type(),
   bm_character_turn_update:type()
)
-> bm_character_turn_update:type().
handle (BattleAction, Update) ->
   case bm_battle_action:get_category(BattleAction) of
      move -> handle_move(BattleAction, Update);
      switch_weapon -> handle_switch_weapon(Update);
      attack -> handle_attack(BattleAction, Update)
   end.
