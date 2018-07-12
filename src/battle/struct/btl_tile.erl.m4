-module(btl_tile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   tile,
   {
      id :: id(),
      name :: binary(),
      cost :: non_neg_integer(),
      class_range_min :: non_neg_integer(),
      class_range_max :: non_neg_integer()
   }
).

-opaque id() :: non_neg_integer().
-opaque class_id() :: non_neg_integer().
-opaque type() :: #tile{}.

-export_type([type/0, class_id/0, id/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_id/1,
      get_name/1,
      get_cost/1,
      get_range_minimum/1,
      get_range_maximum/1,
      from_id/1,
      cost_when_oob/0
   ]
).

-export
(
   [
      class_id_to_type_id/1,
      class_id_from_int/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec class_id_to_type_id (class_id()) -> id().
class_id_to_type_id (ClassID) ->
   case ClassID of
      0 -> 0;
      1 -> 1;
      2 -> 2;
      N when ((N >= 3) and (N =< 17)) -> 3
   end.

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/tile/global.m4.conf)m4_dnl
__TILE_CLASS_USE_ERLANG_STYLE
m4_include(__MAKEFILE_DATA_DIR/tile/grassland.m4d)m4_dnl
from_id(_) ->
   from_id(0).

-spec cost_when_oob () -> non_neg_integer().
cost_when_oob () -> __TILE_COST_WHEN_OOB.

-spec get_id (type()) -> non_neg_integer().
get_id (Tile) -> Tile#tile.id.

-spec get_cost (type()) -> non_neg_integer().
get_cost (Tile) -> Tile#tile.cost.

-spec get_name (type()) -> binary().
get_name (Tile) -> Tile#tile.name.

-spec get_range_minimum (type()) -> non_neg_integer().
get_range_minimum (Tile) -> Tile#tile.class_range_min.

-spec get_range_maximum (type()) -> non_neg_integer().
get_range_maximum (Tile) -> Tile#tile.class_range_max.

-spec class_id_from_int (non_neg_integer()) -> id().
class_id_from_int (I) -> I.
