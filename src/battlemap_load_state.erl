-module(battlemap_load_state).

-export(
   [
      handle/1
   ]
).

handle (Req) ->
   JSONReqMap = jiffy:decode(Req, [return_maps]),
   BattlemapID = maps:get(<<"battlemap_id">>, JSONReqMap),
   io:format("~nLoading Battlemap ~p...", [BattlemapID]),
   Battlemap = timed_cache_object:fetch(battlemaps_db, BattlemapID, 60000),
%%   ok = users_manager:ping(UserToken),
   jiffy:encode(
      {
         [
            {<<"types">>, [<<"STATE">>]},
            {
               <<"data">>,
               [
                  battlemap_battlemap:encode_to_json(Battlemap)
               ]
            }
         ]
      }
   ).
