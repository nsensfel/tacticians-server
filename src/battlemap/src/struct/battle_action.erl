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
-opaque type() :: (#move{} | #switch_weapon{} | #attack{}).

-export_type([category/0, type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      decode/1,
      can_follow/2
   ]
).

-export
(
   [
      get_path/1,
      get_target_ix/1,
      get_category/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode_mov_action (map()) -> type().
decode_mov_action (JSONMap) ->
   PathInBinary = maps:get(<<"p">>, JSONMap),
   Path = lists:map(fun direction:decode/1, PathInBinary),

   #move { path = Path }.

-spec decode_atk_action (map()) -> type().
decode_atk_action (JSONMap) ->
   TargetIX = binary_to_integer(maps:get(<<"tix">>, JSONMap)),

   #attack { target_ix = TargetIX }.

-spec decode_swp_action (map()) -> type().
decode_swp_action (_JSONMap) ->
   #switch_weapon{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode (map()) -> type().
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

-spec get_path (type()) -> list(direction:type()).
get_path (Action) when is_record(Action, move) ->
   Action#move.path;
get_path (_) ->
   [].

-spec get_target_ix (type()) -> non_neg_integer().
get_target_ix (Action) when is_record(Action, attack) ->
   Action#attack.target_ix;
get_target_ix (_) ->
   [].

-spec get_category (type()) -> category().
get_category (Action) when is_record(Action, attack) -> attack;
get_category (Action) when is_record(Action, move) -> move;
get_category (Action) when is_record(Action, switch_weapon) -> switch_weapon.
