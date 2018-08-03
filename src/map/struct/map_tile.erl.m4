-module(map_tile).

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
      family :: non_neg_integer(),
      depth :: non_neg_integer()
   }
).

-opaque class_id() :: non_neg_integer().
-opaque instance() :: (
   {class_id(), class_id(), non_neg_integer()}
   | non_neg_integer()
).

-opaque type() :: #tile{}.

-export_type([type/0, class_id/0, instance/0]).
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
      instance_to_int_list/1,
      instance_from_ints/1
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

-spec extract_main_class_id (instance()) -> class_id().
extract_main_class_id (M) when is_integer(M) ->
   M;
extract_main_class_id ({M, _, _}) ->
   M.

-spec extract_border_class_id (instance()) -> class_id().
extract_border_class_id (M) when is_integer(M) ->
   M;
extract_border_class_id ({_, B, _}) ->
   B.

-spec extract_variant_ix (instance()) -> non_neg_integer().
extract_variant_ix (M) when is_integer(M) ->
   0;
extract_variant_ix ({_, _, V}) ->
   V.

-spec from_class_id (class_id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/tile/global.m4.conf)m4_dnl
__TILE_CLASS_USE_ERLANG_STYLE
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

-spec instance_from_ints
   (
      {non_neg_integer(), non_neg_integer(), non_neg_integer()}
   ) -> instance().
instance_from_ints ({M, B, V}) ->
   case (M == B) of
      true -> M;
      _ -> {M, B, V}
   end.

-spec instance_to_int_list (instance()) -> list(non_neg_integer()).
instance_to_int_list (M) when is_integer(M) -> [M];
instance_to_int_list ({M, B, V}) -> [M, B, V].
