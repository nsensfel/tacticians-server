-module(shr_tile_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type type() ::
   {
      shr_tile:id(),
      shr_tile:variant_id(),
      list(binary()),
      orddict:orddict
      (
         shr_condition:trigger(),
         ordsets:ordset(non_neg_integer())
      )
   }.

-export_type([type/0]).
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
      get_variant_id/1,
      get_triggers/1,

      add_trigger/2, % TODO
      remove_trigger/2, % TODO
      ataxia_add_trigger/2, % TODO
      ataxia_remove_trigger/2 % TODO
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_triggers_field () -> non_neg_integer().
get_triggers_field () -> 3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_tile_id (type()) -> shr_tile:id().
get_tile_id ({TileID, _, _, _}) -> TileID.

-spec get_variant_id (type()) -> shr_tile:variant_id().
get_variant_id ({_, VariantID, _, _}) -> VariantID.

-spec get_triggers (type()) -> list(shr_map_marker:name()).
get_triggers ({_, _, _, Triggers}) -> Triggers.

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

-spec encode
   (
      fun ((shr_map_marker:name()) -> boolean()),
      type()
   )
   -> {list(any())}.
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

-spec add_trigger (shr_map_marker:name(), type()) -> type().
add_trigger (TriggerName, {TileID, VariantID, DisplayData, Triggers}) ->
   {TileID, VariantID, DisplayData, ordsets:add_element(TriggerName, Triggers)}.

-spec ataxia_add_trigger
   (
      shr_map_marker:name(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_trigger (TriggerName, TileInstance) ->
   {
      add_trigger(TriggerName, TileInstance),
      ataxic:update_field
      (
         get_triggers_field(),
         ataxic:apply_function
         (
            ordsets,
            add_element,
            [
               ataxic:constant(TriggerName),
               ataxic:current_value()
            ]
         )
      )
   }.

-spec remove_trigger (shr_map_marker:name(), type()) -> type().
remove_trigger (TriggerName, {TileID, VariantID, DisplayData, Triggers}) ->
   {TileID, VariantID, DisplayData, ordsets:del_element(TriggerName, Triggers)}.

-spec ataxia_remove_trigger
   (
      shr_map_marker:name(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_remove_trigger (TriggerName, TileInstance) ->
   {
      remove_trigger(TriggerName, TileInstance),
      ataxic:update_field
      (
         get_triggers_field(),
         ataxic:apply_function
         (
            ordsets,
            del_element,
            [
               ataxic:constant(TriggerName),
               ataxic:current_value()
            ]
         )
      )
   }.
