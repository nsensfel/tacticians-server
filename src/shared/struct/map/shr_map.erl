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
      markers :: orddict:orddict(shr_map_marker:name(), shr_map_marker:type())
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

      get_related_tile_ids/1
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
      reset_marker/2,
      ataxia_reset_marker/2,
      add_to_marker/3,
      ataxia_add_to_marker/3,
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

-spec get_markers
   (
      type()
   ) -> orddict:orddict(shr_map_marker:name(), shr_map_marker:type()).
get_markers (Map) -> Map#map.markers.

-spec get_marker
   (
      shr_map_marker:name(),
      type()
   )
   -> ('error' | {'ok', shr_map_marker:type()}).
get_marker (Name, Map) ->
   orddict:find(Name, Map#map.markers).

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
-spec get_related_tile_ids (type()) -> ordsets:ordset(shr_tile:id()).
get_related_tile_ids (Map) ->
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
      orddict:orddict(shr_map_marker:name(), shr_map_marker:type()),
      list(shr_tile_instance:type())
   )
   -> type().
update_from_list (Map, Width, Height, Markers, TileInstances) ->
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
      markers = orddict:new(),
      tile_instances = list_to_tuple(lists:duplicate(1024, DefaultTileInstance))
   }.

-spec reset_marker (shr_map_marker:name(), shr_map:type()) -> shr_map:type().
reset_marker (MarkerName, Map) ->
   MapMarkers = Map#map.markers,

   case orddict:find(MarkerName, MapMarkers) of
      error -> Map;
      {ok, Marker} ->
         MapWidth = Map#map.width,
         MarkerLocations = shr_map_marker:get_locations(Marker),
         UpdatedTileInstances =
            lists:foldl
            (
               fun (Location, TileInstances) ->
                  case location_to_index(MapWidth, Location) of
                     error -> TileInstances;
                     Index ->
                        TileInstance = element(Index, TileInstances),

                        UpdatedTileInstance =
                           shr_tile_instance:remove_trigger
                           (
                              MarkerName,
                              TileInstance
                           ),

                        UpdatedTileInstances =
                           setelement
                           (
                              Index,
                              TileInstances,
                              UpdatedTileInstance
                           ),

                        UpdatedTileInstances
                  end
               end,
               Map#map.tile_instances,
               MarkerLocations
            ),
         UpdatedMarker = shr_map_marker:set_locations([], Marker),
         UpdatedMapMarkers =
            orddict:store(MarkerName, UpdatedMarker, MapMarkers),
         UpdatedMap =
            Map#map
            {
               markers = UpdatedMapMarkers,
               tile_instances = UpdatedTileInstances
            },

         UpdatedMap
   end.

-spec ataxia_reset_marker
   (
      shr_map_marker:name(),
      shr_map:type()
   )
   -> {shr_map:type(), ataxic:basic()}.
ataxia_reset_marker (MarkerName, Map) ->
   MapMarkers = Map#map.markers,

   case orddict:find(MarkerName, MapMarkers) of
      error -> {Map, ataxic_sugar:nop()};
      {ok, Marker} ->
         MapWidth = Map#map.width,
         MarkerLocations = shr_map_marker:get_locations(Marker),
         {UpdatedTileInstances, TileInstancesAtaxiaUpdates} =
            lists:foldl
            (
               fun (Location, {TileInstances, TileInstancesAtaxiaUpdates}) ->
                  case location_to_index(MapWidth, Location) of
                     error -> {TileInstances, TileInstancesAtaxiaUpdates};
                     Index ->
                        TileInstance = element(Index, TileInstances),

                        {UpdatedTileInstance, TileInstanceAtaxiaUpdate} =
                           shr_tile_instance:ataxia_remove_trigger
                           (
                              MarkerName,
                              TileInstance
                           ),

                        UpdatedTileInstances =
                           setelement
                           (
                              Index,
                              TileInstances,
                              UpdatedTileInstance
                           ),

                        TileInstancesAtaxiaUpdate =
                           ataxic:update_field
                           (
                              Index,
                              TileInstanceAtaxiaUpdate
                           ),

                        {
                           UpdatedTileInstances,
                           [
                              TileInstancesAtaxiaUpdate
                              |TileInstancesAtaxiaUpdates
                           ]
                        }
                  end
               end,
               {Map#map.tile_instances, []},
               MarkerLocations
            ),
         {UpdatedMarker, MarkerAtaxiaUpdate} =
            shr_map_marker:ataxia_set_locations([], Marker),
         UpdatedMapMarkers =
            orddict:store(MarkerName, UpdatedMarker, MapMarkers),
         MapMarkersAtaxiaUpdate =
            ataxic_sugar:update_orddict_element(MarkerName, MarkerAtaxiaUpdate),
         UpdatedMap =
            Map#map
            {
               markers = UpdatedMapMarkers,
               tile_instances = UpdatedTileInstances
            },

         {
            UpdatedMap,
            ataxic:sequence
            (
               [
                  ataxic:update_field
                  (
                     get_markers_field(),
                     MapMarkersAtaxiaUpdate
                  ),
                  ataxic:update_field
                  (
                     get_tile_instances_field(),
                     ataxic:sequence(TileInstancesAtaxiaUpdates)
                  )
               ]
            )
         }
   end.

-spec add_to_marker
   (
      shr_map_marker:name(),
      list(shr_location:type()),
      shr_map:type()
   )
   -> ({ok, shr_map:type()} | error).
add_to_marker (MarkerName, Locations, Map) ->
   MapMarkers = Map#map.markers,
   case orddict:find(MarkerName, MapMarkers) of
      error -> error;
      {ok, S0Marker} ->
         UpdatedMarker = shr_map_marker:add_locations(Locations, S0Marker),
         UpdatedMapMarkers =
            orddict:store(MarkerName, UpdatedMarker, MapMarkers),
         MapWidth = Map#map.width,
         UpdatedTileInstances =
            lists:foldl
            (
               fun (Location, TileInstances) ->
                  case location_to_index(MapWidth, Location) of
                     error -> TileInstances;
                     Index ->
                        TileInstance = element(Index, TileInstances),

                        UpdatedTileInstance =
                           shr_tile_instance:add_trigger
                           (
                              MarkerName,
                              TileInstance
                           ),
                        UpdatedTileInstances =
                           setelement
                           (
                              Index,
                              TileInstances,
                              UpdatedTileInstance
                           ),

                        UpdatedTileInstances
                  end
               end,
               Map#map.tile_instances,
               Locations
            ),
         UpdatedMap =
            Map#map
            {
               markers = UpdatedMapMarkers,
               tile_instances = UpdatedTileInstances
            },

         {ok, UpdatedMap}
   end.

-spec ataxia_add_to_marker
   (
      shr_map_marker:name(),
      list(shr_location:type()),
      shr_map:type()
   )
   -> ({ok, shr_map:type(), ataxic:basic()} | error).
ataxia_add_to_marker (MarkerName, Locations, Map) ->
   MapMarkers = Map#map.markers,
   case orddict:find(MarkerName, MapMarkers) of
      error -> error;
      {ok, Marker} ->
         MapWidth = Map#map.width,
         {UpdatedTileInstances, TileInstancesAtaxiaUpdates} =
            lists:foldl
            (
               fun (Location, {TileInstances, TileInstancesAtaxiaUpdates}) ->
                  case location_to_index(MapWidth, Location) of
                     error -> {TileInstances, TileInstancesAtaxiaUpdates};
                     Index ->
                        TileInstance = element(Index, TileInstances),

                        {UpdatedTileInstance, TileInstanceAtaxiaUpdate} =
                           shr_tile_instance:ataxia_add_trigger
                           (
                              MarkerName,
                              TileInstance
                           ),

                        UpdatedTileInstances =
                           setelement
                           (
                              Index,
                              TileInstances,
                              UpdatedTileInstance
                           ),

                        TileInstancesAtaxiaUpdate =
                           ataxic:update_field
                           (
                              Index,
                              TileInstanceAtaxiaUpdate
                           ),

                        {
                           UpdatedTileInstances,
                           [
                              TileInstancesAtaxiaUpdate
                              |TileInstancesAtaxiaUpdates
                           ]
                        }
                  end
               end,
               {Map#map.tile_instances, []},
               Locations
            ),
         {UpdatedMarker, MarkerAtaxiaUpdate} =
            shr_map_marker:ataxia_add_locations(Locations, Marker),
         UpdatedMapMarkers =
            orddict:store(MarkerName, UpdatedMarker, MapMarkers),
         MapMarkersAtaxiaUpdate =
            ataxic_sugar:update_orddict_element(MarkerName, MarkerAtaxiaUpdate),
         UpdatedMap =
            Map#map
            {
               markers = UpdatedMapMarkers,
               tile_instances = UpdatedTileInstances
            },

         {
            ok,
            UpdatedMap,
            ataxic:sequence
            (
               [
                  ataxic:update_field
                  (
                     get_markers_field(),
                     MapMarkersAtaxiaUpdate
                  ),
                  ataxic:update_field
                  (
                     get_tile_instances_field(),
                     ataxic:sequence(TileInstancesAtaxiaUpdates)
                  )
               ]
            )
         }
   end.
