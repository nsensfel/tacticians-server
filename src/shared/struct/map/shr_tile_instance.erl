-module(shr_tile_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type display_data() :: list(binary()).

-opaque type() ::
   {
      shr_tile:id(),
      shr_tile:variant_id(),
      display_data(),
      list(shr_map_trigger:id())
   }.

-export_type([type/0, border/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      encode/1,
      decode/1,
      default/0,
      error/0
   ]
).

-export
(
   [
      get_tile_id/1,
      get_variant_id/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_tile_id (type()) -> shr_tile:id().
get_tile_id ({TileID, _, _, _}) -> TileID.

-spec get_variant_id (type()) -> shr_tile:variant_id().
get_variant_id ({_, VariantID, _, _}) -> VariantID.

-spec decode (list(binary())) -> type().
decode (Map) ->
   L = maps:get(<<"b">>, Map),
   T = maps:get(<<"t">>, Map),


   [TileID|[VariantID|DisplayData]] = L,

   S0DisplayData =
      case (((length(DisplayData) rem 2) == 0)) of
         true -> DisplayData;
         _ -> []
      end,

   {TileID, VariantID, S0DisplayData, T}.

-spec encode (instance()) -> list(binary()).
encode (I) -> I.

-spec default () -> type().
default () -> [<<"1">>, <<"0">>].
