-module(bm_tile).

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
      N when ((N >= 3) and (N =< 16)) -> 3
   end.

-spec from_id (id()) -> type().
from_id (0) ->
   #tile
   {
      id = 0,
      name = <<"[Grassland] Grass">>,
      cost = 6,
      class_range_min = 1,
      class_range_max = 1
   };
from_id (1) ->
   #tile
   {
      id = 1,
      name = <<"[Grassland] Mushroom Infestation">>,
      cost = 12,
      class_range_min = 1,
      class_range_max = 1
   };
from_id (2) ->
   #tile
   {
      id = 2,
      name = <<"[Grassland] Tree Remains">>,
      cost = 24,
      class_range_min = 2,
      class_range_max = 2
   };
from_id (3) ->
   #tile
   {
      id = 2,
      name = <<"[Grassland] Clear Water">>,
      cost = cost_when_oob(),
      class_range_min = 3,
      class_range_max = 17
   }.

-spec cost_when_oob () -> non_neg_integer().
cost_when_oob () -> 255.

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
