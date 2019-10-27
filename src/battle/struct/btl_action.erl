-module(btl_action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(CATEGORY_FIELD, <<"cat">>).
-define(CATEGORY_MOVE, <<"mov">>).
-define(CATEGORY_ATTACK, <<"atk">>).
-define(CATEGORY_SWITCH_WEAPONS, <<"swp">>).
-define(CATEGORY_USE_SKILL, <<"skl">>).

-define(MOVE_PATH_FIELD, <<"pat">>).

-define(ATTACK_TARGET_FIELD, <<"tar">>).

-define(USE_SKILL_TARGETS_FIELD, <<"tar">>).
-define(USE_SKILL_LOCATIONS_FIELD, <<"loc">>).

-record
(
   move,
   {
      actor_ix :: non_neg_integer(),
      path :: list(shr_direction:enum()),
      movement_points :: (non_neg_integer() | -1)
   }
).

-record
(
   switch_weapons,
   {
      actor_ix :: non_neg_integer()
   }
).

-record
(
   attack,
   {
      actor_ix :: non_neg_integer(),
      target_ix :: non_neg_integer(),
      is_opportunistic :: boolean()
   }
).

-record
(
   skill,
   {
      actor_ix :: non_neg_integer(),
      targets :: list(non_neg_integer()),
      locations :: list(shr_location:type())
   }
).

-type category() ::
   (
      'move'
      | 'switch_weapons'
      | 'attack'
      | 'skill'
      | 'nothing'
   ).

-opaque type() ::
   (
      #move{}
      | #switch_weapons{}
      | #attack{}
      | #skill{}
   ).

-export_type([category/0, type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      from_map_marker/3,
      can_follow/2
   ]
).

-export
(
   [
      new_move/3,
      new_attack/2,
      new_skill/3,
      new_attack_of_opportunity/2
   ]
).

-export
(
   [
      get_is_opportunistic/1,
      get_path/1,
      get_movement_points/1,
      get_target_index/1,
      get_actor_index/1,
      get_target_indices/1,
      get_locations/1,
      get_category/1
   ]
).

-export
(
   [
      decode/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode (binary(), non_neg_integer(), map()) -> type().
decode (?CATEGORY_MOVE, ActorIX, Map) ->
   EncodedPath = map:get(?MOVE_PATH_FIELD, Map),
   Path = lists:map(fun shr_direction:decode/1, EncodedPath),

   #move
   {
      actor_ix = ActorIX,
      path = Path,
      movement_points = -1
   };
decode (?CATEGORY_ATTACK, ActorIX, Map) ->
   TargetIX = map:get(?ATTACK_TARGET_FIELD, Map),

   #attack
   {
      actor_ix = ActorIX,
      target_ix = TargetIX,
      is_opportunistic = false
   };
decode (?CATEGORY_SWITCH_WEAPONS, ActorIX, _Map) ->
   #switch_weapons
   {
      actor_ix = ActorIX
   };
decode (?CATEGORY_USE_SKILL, ActorIX, Map) ->
   Targets = map:get(?USE_SKILL_TARGETS_FIELD, Map),
   EncodedLocations = map:get(?USE_SKILL_LOCATIONS_FIELD, Map),
   Locations = lists:map(fun shr_location:decode/1, EncodedLocations),

   #skill
   {
      actor_ix = ActorIX,
      targets = Targets,
      locations = Locations
   }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec can_follow (category(), category()) -> boolean().
can_follow (nothing, attack) -> true;
can_follow (nothing, switch_weapons) -> true;
can_follow (nothing, move) -> true;
can_follow (nothing, skill) -> true;
can_follow (move, switch_weapons) -> true;
can_follow (move, attack) -> true;
can_follow (move, skill) -> true;
can_follow (_, _) -> false.

-spec get_path (type()) -> list(shr_direction:type()).
get_path (Action) when is_record(Action, move) -> Action#move.path;
get_path (_) ->
   [].

-spec get_movement_points (type()) -> (non_neg_integer() | -1).
get_movement_points (Action) when is_record(Action, move) ->
   Action#move.movement_points;
get_movement_points (_) -> -1.

-spec get_target_index (type()) -> (non_neg_integer() | -1).
get_target_index (Action) when is_record(Action, attack) ->
   Action#attack.target_ix;
get_target_index (_) -> -1.

-spec get_target_indices (type()) -> list(non_neg_integer()).
get_target_indices (Action) when is_record(Action, skill) ->
   Action#skill.targets;
get_target_indices (_) -> [].

-spec get_locations (type()) -> list(shr_location:type()).
get_locations (Action) when is_record(Action, skill) ->
   Action#skill.locations;
get_locations (_) -> [].

-spec get_actor_index (type()) -> (non_neg_integer() | -1).
get_actor_index (Action) when is_record(Action, attack) ->
   Action#attack.actor_ix;
get_actor_index (Action) when is_record(Action, move) ->
   Action#move.actor_ix;
get_actor_index (Action) when is_record(Action, switch_weapons) ->
   Action#switch_weapons.actor_ix;
get_actor_index (Action) when is_record(Action, skill) ->
   Action#skill.actor_ix;
get_actor_index (_) ->
   -1.

-spec get_is_opportunistic (type()) -> boolean().
get_is_opportunistic (Action) when is_record(Action, attack) ->
   Action#attack.is_opportunistic;
get_is_opportunistic (_) -> false.

-spec new_move
   (
      non_neg_integer(),
      list(shr_direction:type()),
      (non_neg_integer() | -1)
   )
   -> type().
new_move (ActorIX, Path, MovementPoints) ->
   #move
   {
      actor_ix = ActorIX,
      path = Path,
      movement_points = MovementPoints
   }.

-spec new_attack_of_opportunity
   (
      non_neg_integer(),
      non_neg_integer()
   )
   -> type().
new_attack_of_opportunity (ActorIX, TargetIX) ->
   #attack
   {
      actor_ix = ActorIX,
      target_ix = TargetIX,
      is_opportunistic = true
   }.

-spec new_attack (non_neg_integer(), non_neg_integer()) -> type().
new_attack (ActorIX, TargetIX) ->
   #attack
   {
      actor_ix = ActorIX,
      target_ix = TargetIX,
      is_opportunistic = false
   }.

-spec new_skill
   (
      non_neg_integer(),
      list(non_neg_integer()),
      list(shr_location:type())
   )
   -> type().
new_skill (ActorIX, Targets, Locations) ->
   #skill
   {
      actor_ix = ActorIX,
      targets = Targets,
      locations = Locations
   }.

-spec get_category (type()) -> category().
get_category (Action) when is_record(Action, attack) -> attack;
get_category (Action) when is_record(Action, move) -> move;
get_category (Action) when is_record(Action, switch_weapons) -> switch_weapons;
get_category (Action) when is_record(Action, skill) -> skill.

-spec decode (non_neg_integer(), map()) -> type().
decode (ActorIX, Map) -> decode(map:get(?CATEGORY_FIELD), ActorIX, Map).

-spec from_map_marker
   (
      non_neg_integer(),
      btl_character:type(),
      shr_map_marker:type()
   )
   -> list(type()).
from_map_marker (CharacterIX, _Character, Marker) ->
   case shr_map_marker:get_category(Marker) of
      matk ->
         [
            #attack
            {
               target_ix = CharacterIX,
               actor_ix = shr_map_marker:get_character_index(Marker),
               is_opportunistic = true
            }
         ];

      _ -> []
   end.
