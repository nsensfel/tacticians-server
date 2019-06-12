-module(shr_map_marker).

-define(OWNER_IX_FIELD, <<"oix">>).
-define(LOCATIONS_FIELD, <<"l">>).
-define(DATA_FIELD, <<"d">>).

-define(DATA_TYPE_FIELD, <<"t">>).
-define(MATK_TYPE_VALUE, <<"matk">>).
-define(SPAWN_TYPE_VALUE, <<"spawn">>).

-define(MATK_CHARACTER_IX_FIELD, <<"cix">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   matk_mrk,
   {
      character_ix :: non_neg_integer()
   }
).

-record
(
   spawn_mrk,
   {
   }
).

-record
(
   marker,
   {
      owner_ix :: (non_neg_integer() | -1),
      data :: (data()),
      locations :: list(shr_location:type())
   }
).

-type name() :: binary().
-type category() :: (matk | spawn).
-opaque melee_attack_zone() :: #matk_mrk{}.
-opaque spawn_zone() :: #spawn_mrk{}.
-opaque data() :: (#matk_mrk{} | #spawn_mrk{}).
-opaque type() :: #marker{}.

-export_type
(
   [
      name/0,
      type/0,
      category/0,
      data/0,
      melee_attack_zone/0,
      spawn_zone/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      player_can_see/2,
      get_locations/1,
      get_name/1,
      get_owner_index/1,
      get_category/1,
      interrupts_movement/2
   ]
).

-export
(
   [
      get_character_index/1
   ]
).

-export
(
   [
      set_locations/2,
      add_locations/2,
      remove_locations/2,
      ataxia_set_locations/2,
      ataxia_add_locations/2,
      ataxia_remove_locations/2
   ]
).

-export
(
   [
      decode/1,
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec encode_data (data()) -> {list({binary(), any()})}.
encode_data (MarkerData) when is_record(MarkerData, matk_mrk) ->
   {
      [
         { ?DATA_TYPE_FIELD, ?MATK_TYPE_VALUE },
         { ?MATK_CHARACTER_IX_FIELD, MarkerData#matk_mrk.character_ix }
      ]
   };
encode_data (MarkerData) when is_record(MarkerData, spawn_mrk) ->
   {
      [
         { ?DATA_TYPE_FIELD, ?SPAWN_TYPE_VALUE }
      ]
   }.

-spec decode_data (map()) -> data().
decode_data (Map) ->
   case maps:get(?DATA_TYPE_FIELD, Map) of
      ?MATK_TYPE_VALUE ->
         #matk_mrk
         {
            character_ix = maps:get(?MATK_CHARACTER_IX_FIELD, Map)
         };

      ?SPAWN_TYPE_VALUE -> #spawn_mrk{};

      _Other ->
         % TODO: error.
         #spawn_mrk{}
   end.

-spec get_locations_field () -> non_neg_integer().
get_locations_field () -> #marker.locations.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec interrupts_movement (non_neg_integer(), type()) -> boolean().
interrupts_movement (PlayerIX, Marker) ->
   (
      (PlayerIX /= Marker#marker.owner_ix)
      and is_record(Marker#marker.data, matk_mrk)
   ).

-spec get_locations (type()) -> list(shr_location:type()).
get_locations (Marker) -> Marker#marker.locations.

-spec get_owner_index (type()) -> (non_neg_integer() | -1).
get_owner_index (Marker) -> Marker#marker.owner_ix.

-spec get_category (type()) -> category().
get_category (Marker) ->
   case Marker#marker.data of
      #matk_mrk{} -> matk;
      #spawn_mrk{} -> spawn
   end.

-spec encode (type()) -> {list({binary(), any()})}.
encode (Marker) ->
   {
      [
         {
            ?LOCATIONS_FIELD,
            lists:map(fun shr_location:encode/1, Marker#marker.locations)
         },
         { ?OWNER_IX_FIELD, Marker#marker.owner_ix },
         { ?DATA_FIELD, encode_data(Marker#marker.data) }
      ]
   }.


-spec decode (map()) -> type().
decode (Map) ->
   #marker
   {
      locations = maps:get(?LOCATIONS_FIELD, Map),
      owner_ix = maps:get(?OWNER_IX_FIELD, Map),
      data = decode_data(maps:get(?DATA_FIELD, Map))
   }.

-spec player_can_see (integer(), type()) -> boolean().
player_can_see (_PlayerIX, _Marker) -> true.

-spec get_name (type()) -> binary().
get_name (Marker) ->
   case Marker#marker.data of
      #matk_mrk{ character_ix = CIX } ->
         Prefix = <<"matk_c">>,
         CharacterIXString = integer_to_binary(CIX),
         <<Prefix/binary, CharacterIXString/binary>>;

      #spawn_mrk{} ->
         Prefix = <<"spawn_p">>,
         PlayerIXString = integer_to_binary(Marker#marker.owner_ix),
         <<Prefix/binary, PlayerIXString/binary>>
   end.

-spec get_character_index (type()) -> (non_neg_integer() | -1).
get_character_index (Marker) ->
   case Marker#marker.data of
      #matk_mrk{ character_ix = IX } -> IX;
      _ -> -1
   end.

-spec set_locations (list(shr_location:type()), type()) -> type().
set_locations (Locations, Marker) -> Marker#marker{ locations = Locations }.

-spec ataxia_set_locations
   (
      list(shr_location:type()),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_locations (Locations, Marker) ->
   {
      set_locations(Locations, Marker),
      ataxic:update_field
      (
         get_locations_field(),
         ataxic:constant([Locations])
      )
   }.

-spec add_locations (list(shr_location:type()), type()) -> type().
add_locations (Locations, Marker) ->
   Marker#marker{ locations = (Locations ++ Marker#marker.locations) }.

-spec ataxia_add_locations
   (
      list(shr_location:type()),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_locations (Locations, Marker) ->
   {
      add_locations(Locations, Marker),
      ataxic:update_field
      (
         get_locations_field(),
         ataxic:apply_function
         (
            lists,
            append,
            [
               ataxic:constant([Locations]),
               ataxic:current_value()
            ]
         )
      )
   }.

-spec remove_locations (list(shr_location:type()), type()) -> type().
remove_locations (Locations, Marker) ->
   Marker#marker
   {
      locations = lists:subtract(Marker#marker.locations, Locations)
   }.

-spec ataxia_remove_locations
   (
      list(shr_location:type()),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_remove_locations (Locations, Marker) ->
   {
      remove_locations(Locations, Marker),
      ataxic:update_field
      (
         get_locations_field(),
         ataxic:apply_function
         (
            lists,
            subtract,
            [
               ataxic:current_value(),
               ataxic:constant([Locations])
            ]
         )
      )
   }.
