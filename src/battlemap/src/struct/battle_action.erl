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

-export_type([category/0, struct/0]).

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
