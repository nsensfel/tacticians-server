-module(shr_map_marker).

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
      player_ix :: non_neg_integer()
   }
).

-type name() :: binary().
-opaque melee_attack_zone() :: #matk_mrk{}.
-opaque spawn_zone() :: #spawn_mrk{}.
-opaque type() ::
   {list(shr_location:type()), (melee_attack_zone() | spawn_zone())}.

-export_type([name/0, type/0, melee_attack_zone/0, spawn_zone/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      player_can_see/2,
      get_locations/1
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_locations (type()) -> list(shr_location:type()).
get_locations ({L, _}) -> L.

-spec encode (type()) -> {list(any())}.
encode ({L, MarkerData}) when is_record(MarkerData, matk_mrk) ->
   {
      [
         { <<"t">>, <<"matk">> },
         { <<"cix">>, MarkerData#matk_mrk.character_ix },
         { <<"l">>, lists:map(fun shr_location:encode/1, L) }
      ]
   };
encode ({L, MarkerData}) when is_record(MarkerData, spawn_mrk) ->
   {
      [
         { <<"t">>, <<"spawn">> },
         { <<"pix">>, MarkerData#spawn_mrk.player_ix },
         { <<"l">>, lists:map(fun shr_location:encode/1, L) }
      ]
   }.

-spec decode (map()) -> type().
decode (Map) ->
   Data = maps:get(<<"d">>, Map),
   {
      lists:map(fun shr_location:decode/1, maps:get(<<"l">>, Map)),
      (
         case maps:get(<<"t">>, Data) of
            <<"mtak">> -> #matk_mrk{ character_ix = maps:get(<<"cix">>, Data) };
            <<"spawn">> -> #spawn_mrk{ player_ix = maps:get(<<"pix">>, Data) }
         end
      )
   }.

-spec player_can_see (integer(), type()) -> boolean().
player_can_see (IX, _Marker) -> (IX >= 0).
