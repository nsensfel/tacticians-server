-module(shr_tile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   tile,
   {
      id :: class_id(),
      name :: binary(),
      cost :: non_neg_integer(),
      omnimods :: shr_omnimods:type(),
      family :: non_neg_integer(),
      depth :: non_neg_integer()
   }
).

-type class_id() :: non_neg_integer().
-opaque instance() :: list(non_neg_integer()).
-opaque border() :: list(non_neg_integer()).

-opaque type() :: #tile{}.

-export_type([type/0, class_id/0, instance/0, border/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_class_id/1,
      get_name/1,
      get_cost/1,
      get_omnimods/1,
      from_class_id/1,
      cost_when_oob/0
   ]
).

-export
(
   [
      instance_to_int_list/1,
      instance_from_ints/1,
      default_tile_instance/0
   ]
).

-export
(
   [
      extract_main_class_id/1,
      extract_variant_ix/1,
      extract_borders/1
   ]
).

-export
(
   [
      extract_border_main_class_id/1,
      extract_border_variant_ix/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec extract_main_class_id (instance()) -> class_id().
extract_main_class_id (I) -> lists:nth(1, I).

-spec extract_borders (instance()) -> list(border()).
extract_borders (I) ->
   [_|[_|Result]] = I,
   Result.

-spec extract_variant_ix (instance()) -> non_neg_integer().
extract_variant_ix (I) -> lists:nth(2, I).

-spec extract_border_main_class_id (border()) -> class_id().
extract_border_main_class_id (B) -> lists:nth(1, B).

-spec extract_border_variant_ix (border()) -> non_neg_integer().
extract_border_variant_ix (B) -> lists:nth(2, B).

-spec from_class_id (class_id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/tile/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/error.m4d)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/grassland.m4d)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/mud.m4d)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/tile/water.m4d)m4_dnl
from_class_id(_) ->
   from_class_id(0).

-spec cost_when_oob () -> non_neg_integer().
cost_when_oob () -> __TILE_COST_WHEN_OOB.

-spec get_class_id (type()) -> non_neg_integer().
get_class_id (Tile) -> Tile#tile.id.

-spec get_cost (type()) -> non_neg_integer().
get_cost (Tile) -> Tile#tile.cost.

-spec get_name (type()) -> binary().
get_name (Tile) -> Tile#tile.name.

-spec get_omnimods (type()) -> shr_omnimods:type().
get_omnimods (Tile) -> Tile#tile.omnimods.

-spec instance_from_ints (list(non_neg_integer())) -> instance().
instance_from_ints (L) ->
   LLength = length(L),

   case (((LLength rem 2) == 0) and (LLength /= 0)) of
      true -> L;
      _ -> [0, 0]
   end.

-spec instance_to_int_list (instance()) -> list(non_neg_integer()).
instance_to_int_list (I) -> I.

-spec default_tile_instance () -> instance().
default_tile_instance () -> [1, 0].
