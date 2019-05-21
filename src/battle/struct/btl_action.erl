-module(btl_action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   move,
   {
      path :: list(shr_direction:enum())
   }
).

-record
(
   interrupted_move,
   {
      path :: list(shr_direction:enum()),
      movement_points :: non_neg_integer()
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

-record
(
   defend,
   {
      target_ix :: non_neg_integer()
   }
).


-type category() ::
   (
      'move'
      | 'interrupted_move'
      | 'switch_weapon'
      | 'attack'
      | 'defend'
      | 'nothing'
   ).
-opaque type() ::
   (
      #move{}
      | #interrupted_move{}
      | #switch_weapon{}
      | #attack{}
      | #defend{}
   ).

-export_type([category/0, type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      from_map_marker/2,
      maybe_decode_move/1,
      maybe_decode_weapon_switch/1,
      maybe_decode_attack/1,
      can_follow/2
   ]
).

-export
(
   [
      new_interrupted_move/2
   ]
).

-export
(
   [
      get_path/1,
      get_movement_points/1,
      get_target_ix/1,
      get_category/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec maybe_decode_move (list(shr_direction:type())) -> list(type()).
maybe_decode_move ([]) -> [];
maybe_decode_move (PathInBinary) ->
   Path = lists:map(fun shr_direction:decode/1, PathInBinary),

   [#move{ path = Path }].

-spec maybe_decode_attack (integer()) -> list(type()).
maybe_decode_attack (TargetIX) when (TargetIX < 0) -> [];
maybe_decode_attack (TargetIX) -> [#attack{ target_ix = TargetIX }].

-spec maybe_decode_weapon_switch (boolean()) -> list(type()).
maybe_decode_weapon_switch (false) -> [];
maybe_decode_weapon_switch (true) -> [#switch_weapon{}].

-spec can_follow (category(), category()) -> boolean().
can_follow (nothing, attack) -> true;
can_follow (nothing, switch_weapon) -> true;
can_follow (nothing, move) -> true;
can_follow (move, switch_weapon) -> true;
can_follow (move, attack) -> true;
can_follow (_, _) -> false.

-spec get_path (type()) -> list(shr_direction:type()).
get_path (Action) when is_record(Action, move) ->
   Action#move.path;
get_path (Action) when is_record(Action, interrupted_move) ->
   Action#interrupted_move.path;
get_path (_) ->
   [].

-spec get_movement_points (type()) -> non_neg_integer().
get_movement_points (Action) when is_record(Action, interrupted_move) ->
   Action#interrupted_move.movement_points;
get_movement_points (_) -> 0.

-spec get_target_ix (type()) -> non_neg_integer().
get_target_ix (Action) when is_record(Action, attack) ->
   Action#attack.target_ix;
get_target_ix (Action) when is_record(Action, defend) ->
   Action#defend.target_ix;
get_target_ix (_) ->
   0.

-spec new_interrupted_move
   (
      list(shr_direction:type()),
      non_neg_integer()
   )
   -> type().
new_interrupted_move (Path, MovementPoints) ->
   #interrupted_move{ path = Path, movement_points = MovementPoints }.

-spec get_category (type()) -> category().
get_category (Action) when is_record(Action, attack) -> attack;
get_category (Action) when is_record(Action, move) -> move;
get_category (Action) when is_record(Action, switch_weapon) -> switch_weapon;
get_category (Action) when is_record(Action, interrupted_move) ->
   interrupted_move;
get_category (Action) when is_record(Action, defend) ->
  defend.

-spec from_map_marker
   (
      btl_character:type(),
      shr_map_marker:type()
   )
   -> list(type()).
from_map_marker (_Character, Marker) ->
   case shr_map_marker:get_category(Marker) of
      matk ->
         [
            #defend
            {
               target_ix = shr_map_marker:get_character_index(Marker)
            }
         ];

      _ -> []
   end.
