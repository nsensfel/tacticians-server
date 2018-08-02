-module(btl_tile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   tile,
   {
      id :: class_id(),
      name :: binary(),
      cost :: non_neg_integer()
   }
).

-opaque class_id() :: non_neg_integer().
-opaque id() :: {class_id(), class_id(), non_neg_integer()}.

-opaque type() :: #tile{}.

-export_type([type/0, class_id/0, id/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_class_id/1,
      get_name/1,
      get_cost/1,
      from_class_id/1,
      cost_when_oob/0
   ]
).

-export
(
   [
      id_to_int_list/1,
      id_from_ints/1
   ]
).

-export
(
   [
      extract_main_class_id/1,
      extract_border_class_id/1,
      extract_variant_ix/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec extract_main_class_id (id()) -> class_id().
extract_main_class_id ({M, _, _}) -> M.

-spec extract_border_class_id (id()) -> class_id().
extract_border_class_id ({_, B, _}) -> B.

-spec extract_variant_ix (id()) -> class_id().
extract_variant_ix ({_, _, V}) -> V.

-spec from_class_id (class_id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/tile/global.m4.conf)m4_dnl
__TILE_CLASS_USE_ERLANG_STYLE
m4_include(__MAKEFILE_DATA_DIR/tile/grassland.m4d)m4_dnl
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

-spec id_from_ints
   (
      {non_neg_integer(), non_neg_integer(), non_neg_integer()}
   ) -> id().
id_from_ints ({M, B, V}) -> {M, B, V}.

-spec id_to_int_list (id()) -> list(non_neg_integer()).
id_to_int_list ({M, B, V}) -> [M, B, V].
