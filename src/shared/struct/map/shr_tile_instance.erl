-module(shr_tile_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type display_data() :: list(binary()).
-type trigger_name() :: binary().

-opaque type() ::
   {
      shr_tile:id(),
      shr_tile:variant_id(),
      display_data(),
      list(trigger_name())
   }.

-export_type([type/0, trigger_name/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      encode/2,
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

-spec decode (map()) -> type().
decode (Map) ->
   L = maps:get(<<"b">>, Map),
   Triggers = maps:get(<<"t">>, Map),

   [TileID|[VariantID|DisplayData]] = L,

   S0DisplayData =
      case (((length(DisplayData) rem 2) == 0)) of
         true -> DisplayData;
         _ -> []
      end,

   {TileID, VariantID, S0DisplayData, Triggers}.

-spec encode (fun ((trigger_name()) -> boolean()), type()) -> {list(any())}.
encode (VisibilityFun, {TileID, VariantID, DisplayData, Triggers}) ->
   {
      [
         {<<"b">>, [TileID|[VariantID|DisplayData]]},
         {<<"t">>, lists:filter(VisibilityFun, Triggers)}
      ]
   }.

-spec default () -> type().
default () -> {<<"1">>, <<"0">>, [], []}.

-spec error () -> type().
error () -> {<<"0">>, <<"0">>, [], []}.
