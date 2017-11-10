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
   {Battlemap, CharList} =
      timed_cache_object:fetch(
         battlemaps_db,
         BattlemapID
      ),
%%   ok = users_manager:ping(UserToken),
   jiffy:encode(
      {
         [
            {
               <<"types">>,
               [
                  <<"SET_MAP">>,
                  lists:map(
                     fun (_Char) ->
                        <<"ADD_CHAR">>
                     end,
                     CharList
                  )
               ]
            },
            {
               <<"data">>,
               [
                  battlemap_battlemap:encode_to_json(Battlemap)
                  |
                  lists:map(
                     fun (Char) ->
                        battlemap_character:encode_in_json(Char)
                     end,
                     CharList
                  )
               ]
            }
         ]
      }
   ).
