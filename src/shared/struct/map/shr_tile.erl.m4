-module(shr_tile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().
-type variant_id() :: binary().

-record
(
   tile,
   {
      id :: id(),
      name :: binary(),
      cost :: non_neg_integer(),
      omnimods :: shr_omnimods:type(),
      family :: variant_id(),
      depth :: non_neg_integer()
   }
).

-opaque type() :: #tile{}.

-export_type([type/0, id/0, variant_id/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_id/1,
      get_name/1,
      get_cost/1,
      get_omnimods/1,
      from_id/1,
      cost_when_oob/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/tile/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/special.m4d)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/grassland.m4d)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/mud.m4d)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/water.m4d)m4_dnl
from_id(_) ->
   from_id(<<"0">>).

-spec cost_when_oob () -> non_neg_integer().
cost_when_oob () -> __TILE_COST_WHEN_OOB.

-spec get_id (type()) -> class_id().
get_id (Tile) -> Tile#tile.id.

-spec get_cost (type()) -> non_neg_integer().
get_cost (Tile) -> Tile#tile.cost.

-spec get_name (type()) -> binary().
get_name (Tile) -> Tile#tile.name.

-spec get_omnimods (type()) -> shr_omnimods:type().
get_omnimods (Tile) -> Tile#tile.omnimods.
