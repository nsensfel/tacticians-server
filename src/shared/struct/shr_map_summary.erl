-module(shr_map_summary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   map_summary,
   {
      id :: shr_map:id(),
      name :: binary()
   }
).

-opaque type() :: #map_summary{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      new/2
   ]
).

%%%% Accessors
-export
(
   [
      get_id/1,
      get_name/1
   ]
).
-export
(
   [
      set_id/2,
      set_name/2
   ]
).

-export
(
   [
      get_id_field/0,
      get_name_field/0
   ]
).

%%%% Export
-export
(
   [
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (shr_map:id(), binary()) -> type().
new (ID, Name) ->
   #map_summary
   {
      id = ID,
      name = Name
   }.

%%%% Accessors
-spec get_id (type()) -> shr_map:id().
get_id (MapSummary) -> MapSummary#map_summary.id.

-spec get_name (type()) -> binary().
get_name (MapSummary) -> MapSummary#map_summary.name.

-spec set_id (shr_map:id(), type()) -> type().
set_id (Val, MapSummary) -> MapSummary#map_summary{ id = Val }.

-spec set_name (binary(), type()) -> type().
set_name (Val, MapSummary) -> MapSummary#map_summary{ name = Val }.

-spec get_id_field () -> non_neg_integer().
get_id_field () -> #map_summary.id.

-spec get_name_field () -> non_neg_integer().
get_name_field () -> #map_summary.name.

-spec encode ({non_neg_integer(), type()}) -> {list(any())}.
encode ({IX, MapSummary}) ->
   {
      [
         {<<"ix">>, IX},
         {<<"id">>, MapSummary#map_summary.id},
         {<<"nme">>, MapSummary#map_summary.name}
      ]
   }.
