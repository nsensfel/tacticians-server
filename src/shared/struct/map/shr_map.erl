-module(shr_map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: ataxia_id:type().

-record
(
   map,
   {
      owner :: shr_player:id(),
      width :: non_neg_integer(),
      height :: non_neg_integer(),
      tile_instances :: shr_array_tuple:array_tuple(shr_tile_instance:type()),
      markers :: shr_map_marker:collection()
   }
).

-opaque type() :: #map{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_owner/1,
      get_width/1,
      get_height/1,
      get_tile_instances/1,
      get_tile_instance/2,
      get_markers/1,
      get_marker/2,

      get_used_tile_ids/1
   ]
).

%%%% Fields
-export
(
   [
      get_width_field/0,
      get_height_field/0,
      get_markers_field/0,
      get_tile_instances_field/0
   ]
).

%%%% Utility
-export
(
   [
      update_from_list/5,
      default/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec location_to_index
   (
      non_neg_integer(),
      shr_location:type()
   )
   -> ('error' | non_neg_integer()).
location_to_index (ArrayWidth, {X, Y}) ->
   if
      (X < 0) -> error;
      (Y < 0) -> error;
      (X >= ArrayWidth) -> error;
      true -> ((Y * ArrayWidth) + X)
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_owner (type()) -> shr_player:id().
get_owner (Map) -> Map#map.owner.

-spec get_width (type()) -> non_neg_integer().
get_width (Map) -> Map#map.width.

-spec get_height (type()) -> non_neg_integer().
get_height (Map) -> Map#map.height.

-spec get_tile_instances
   (
      type()
   )
   -> shr_array_tuple:array_tuple(shr_tile_instance:type()).
get_tile_instances (Map) -> Map#map.tile_instances.

-spec get_tile_instance
   (
      shr_location:type(),
      type()
   )
   -> shr_tile_instance:type().
get_tile_instance (Location, Map) ->
   TileIX = location_to_index(Map#map.width, Location),
   element((TileIX + 1), Map#map.tile_instances).

-spec get_markers (type()) -> shr_map_marker:collection().
get_markers (Map) -> Map#map.markers.

-spec get_marker
   (
      shr_map_marker:name(),
      type()
   )
   -> ('not_found' | {'ok', shr_map_marker:type()}).
get_marker (Name, Map) ->
   shr_map_marker:get(Name, Map#map.markers).

%%%% Fields
-spec get_width_field () -> non_neg_integer().
get_width_field () -> #map.width.

-spec get_height_field () -> non_neg_integer().
get_height_field () -> #map.height.

-spec get_markers_field () -> non_neg_integer().
get_markers_field () -> #map.markers.

-spec get_tile_instances_field () -> non_neg_integer().
get_tile_instances_field () -> #map.tile_instances.

%%%% Utility
-spec get_used_tile_ids (type()) -> ordsets:ordset(shr_tile:id()).
get_used_tile_ids (Map) ->
   UsedTileIDs =
      lists:foldl
      (
         fun (TileInstance, CurrentTileIDs) ->
            ordsets:add_element
            (
               shr_tile_instance:get_tile_id(TileInstance),
               CurrentTileIDs
            )
         end,
         ordsets:new(),
         tuple_to_list(Map#map.tile_instances)
      ),

   UsedTileIDs.

-spec update_from_list
   (
      type(),
      non_neg_integer(),
      non_neg_integer(),
      shr_map_marker:collection(),
      list(map())
   )
   -> type().
update_from_list (Map, Width, Height, Markers, List) ->
   TileInstances = lists:map(fun shr_tile_instance:decode/1, List),

   Map#map
   {
      width = Width,
      height = Height,
      markers = Markers,
      tile_instances = list_to_tuple(TileInstances)
   }.

-spec default (binary()) -> type().
default (Owner) ->
   DefaultTileInstance = shr_tile_instance:default(),

   #map
   {
      owner = Owner,
      width = 32,
      height = 32,
      markers = shr_map_marker:empty_collection(),
      tile_instances = list_to_tuple(lists:duplicate(1024, DefaultTileInstance))
   }.
