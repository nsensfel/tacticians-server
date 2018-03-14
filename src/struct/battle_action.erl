-module(battle_action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   move,
   {
      path :: list(direction:enum())
   }
).

-record
(
   switch_weapon,
   {
   }
).

-record
(
   attack,
   {
      target_ix :: non_neg_integer()
   }
).

-type category() :: ('move' | 'switch_weapon' | 'attack' | 'nothing').
-opaque struct() :: (#move{} | #switch_weapon{} | #attack{}).

-export_type([category/0, struct/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      decode/1,
      handle/4,
      can_follow/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode_mov_action (map()) -> struct().
decode_mov_action (JSONMap) ->
   PathInBinary = maps:get(<<"p">>, JSONMap),
   Path = lists:map(fun direction:decode/1, PathInBinary),

   #move { path = Path }.

-spec decode_atk_action (map()) -> struct().
decode_atk_action (JSONMap) ->
   TargetIX = binary_to_integer(maps:get(<<"tix">>, JSONMap)),

   #attack { target_ix = TargetIX }.

-spec decode_swp_action (map()) -> struct().
decode_swp_action (_JSONMap) ->
   #switch_weapon{}.

-spec handle_attack_sequence
   (
      character_instance:struct(),
      character_instance:struct(),
      list(attack:step())
   )
   -> {list(attack:struct()), non_neg_integer(), non_neg_integer()}.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode (map()) -> struct().
decode (EncodedAction) ->
   JSONActionMap = EncodedAction, %jiffy:decode(EncodedAction, [return_maps]),
   ActionType = maps:get(<<"t">>, JSONActionMap),
   case ActionType of
      <<"mov">> -> decode_mov_action(JSONActionMap);
      <<"atk">> -> decode_atk_action(JSONActionMap);
      <<"swp">> -> decode_swp_action(JSONActionMap)
   end.

-spec can_follow (category(), category()) -> boolean().
can_follow (nothing, attack) -> true;
can_follow (nothing, switch_weapon) -> true;
can_follow (nothing, move) -> true;
can_follow (switch_weapon, attack) -> true;
can_follow (move, attack) -> true;
can_follow (_, _) -> false.

-spec handle
(
   battle:struct(),
   character_instance:struct(),
   non_neg_integer(),
   struct()
)
->
{
   list(database_diff:struct()),
   list(turn_result:struct()),
   battle:struct(),
   character_instance:struct()
}.
handle (Battle, CharacterInstance, CharacterInstanceIX, BattleAction)
when is_record(BattleAction, switch_weapon) ->
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

   {
      % TODO: hide that into database_diff structs.
      [
         {character_instance, CharacterInstanceIX, wp0, SecondaryWeaponID},
         {character_instance, CharacterInstanceIX, wp1, PrimaryWeaponID}
         % ... statistics as well.
      ],
      [turn_result:new_character_switched_weapons(CharacterInstanceIX)],
      Battle,
      UpdatedCharacterInstance
   };
handle (Battle, CharacterInstance, CharacterInstanceIX, BattleAction)
when is_record(BattleAction, move) ->
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
            case IX of
               CharacterInstanceIX -> Prev;
               _ -> [character_instance:get_location(CharInst)|Prev]
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
   };
handle (Battle, CharacterInstance, CharacterInstanceIX, BattleAction)
when is_record(BattleAction, attack) ->
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
