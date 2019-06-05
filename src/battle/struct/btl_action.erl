-module(btl_action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
   switch_weapon,
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

-type category() ::
   (
      'move'
      | 'switch_weapon'
      | 'attack'
      | 'nothing'
   ).

-opaque type() ::
   (
      #move{}
      | #switch_weapon{}
      | #attack{}
   ).

-export_type([category/0, type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      from_map_marker/3,
      maybe_decode_move/2,
      maybe_decode_weapon_switch/2,
      maybe_decode_attack/2,
      can_follow/2
   ]
).

-export
(
   [
      new_move/3
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
      get_category/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec maybe_decode_move
   (
      non_neg_integer(),
      list(shr_direction:type())
   )
   -> list(type()).
maybe_decode_move (_CharacterIX, []) -> [];
maybe_decode_move (CharacterIX, PathInBinary) ->
   Path = lists:map(fun shr_direction:decode/1, PathInBinary),

   [
      #move
      {
         actor_ix = CharacterIX,
         path = Path,
         movement_points = -1
      }
   ].

-spec maybe_decode_attack
   (
      non_neg_integer(),
      integer()
   )
   -> list(type()).
maybe_decode_attack (_CharacterIX, TargetIX) when (TargetIX < 0) -> [];
maybe_decode_attack (CharacterIX, TargetIX) ->
   [
      #attack
      {
         actor_ix = CharacterIX,
         target_ix = TargetIX,
         is_opportunistic = false
      }
   ].

-spec maybe_decode_weapon_switch
   (
      non_neg_integer(),
      boolean()
   )
   -> list(type()).
maybe_decode_weapon_switch (_CharacterIX, false) -> [];
maybe_decode_weapon_switch (CharacterIX, true) ->
   [#switch_weapon{ actor_ix = CharacterIX }].

-spec can_follow (category(), category()) -> boolean().
can_follow (nothing, attack) -> true;
can_follow (nothing, switch_weapon) -> true;
can_follow (nothing, move) -> true;
can_follow (move, switch_weapon) -> true;
can_follow (move, attack) -> true;
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

-spec get_actor_index (type()) -> (non_neg_integer() | -1).
get_actor_index (Action) when is_record(Action, attack) ->
   Action#attack.actor_ix;
get_actor_index (Action) when is_record(Action, move) ->
   Action#move.actor_ix;
get_actor_index (Action) when is_record(Action, switch_weapon) ->
   Action#switch_weapon.actor_ix;
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

-spec get_category (type()) -> category().
get_category (Action) when is_record(Action, attack) -> attack;
get_category (Action) when is_record(Action, move) -> move;
get_category (Action) when is_record(Action, switch_weapon) -> switch_weapon.

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
