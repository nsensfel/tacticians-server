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
      target :: non_neg_integer()
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
   Path = lists:map(fun direction:from_binary/1, PathInBinary),

   #move { path = Path }.

-spec decode_atk_action (map()) -> struct().
decode_atk_action (JSONMap) ->
   TargetIX = maps:get(<<"tix">>, JSONMap),

   #attack { target = TargetIX }.

-spec decode_swp_action (map()) -> struct().
decode_swp_action (_JSONMap) ->
   #switch_weapon{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode (binary()) -> struct().
decode (EncodedAction) ->
   JSONActionMap = jiffy:decode(EncodedAction, [return_maps]),
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
      % TODO: hide that into turn_result structs.
      [
         {switched_weapons, CharacterInstanceIX}
      ],
      UpdatedCharacterInstance,
      Battle
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
      [
         {moved, Path, NewLocation}
      ],
      UpdatedCharacterInstance,
      Battle
   }.
handle (Battle, CharacterInstance, CharacterInstanceIX, BattleAction)
when is_record(BattleAction, attack) ->
   Character = character_instance:get_character(CharacterInstance),
   CharacterStatistics = character:get_statistics(Character),
   Battlemap = battle:get_battlemap(Battle),
   TargetIX = BattleAction#attack.target_ix,
   TargetCharacterInstance = battle:get_character_instance(TargetIX, Battle),
   TargetCharacter = character_instance:get_character(TargetCharacterInstance),
   TargetCharacterStatistics = character:get_statistics(TargetCharacter),

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

   % FIXME: may warrant a separate function
   {AttackEffects, RemainingAttakerHealth, RemainingDefenderHealth} =
      lists:foldl
      (
         fun
         (
            AttackEffectCandidate,
            {AttackValidEffects, AttackerHealth, DefenderHealth }
         ) ->
            {AttackResult, NewAttackerHealth, NewDefenderHealth} =
               attack:apply_to_healths
               (
                  AttackPlannedEffect,
                  AttackerHealth,
                  DefenderHealth
               ),
            case AttackResult of
               nothing -> {AttackValidEffects, AttackerHealth, DefenderHealth};
               _ ->
                  {
                     [AttackResult|AttackValidEffects],
                     NewAttackerHealth,
                     NewDefenderHealth
                  }
            end,
         end,
         {
            [],
            character_instance:get_current_health(CharacterInstance),
            character_instance:get_current_health(TargetCharacterInstance)
         },
         AttackPlannedEffects
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
      )
   {
      % TODO: hide that into database_diff structs.
      [], % TODO
      % TODO: hide that into turn_result structs.
      AttackEffets,
      UpdatedCharacterInstance,
      UpdatedBattle
   }.
