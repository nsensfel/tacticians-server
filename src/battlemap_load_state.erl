-module(battlemap_load_state).

-export(
   [
      handle/1
   ]
).

handle (Req) ->
   JSONReqMap = jiffy:decode(Req, [return_maps]),
   BattlemapID = maps:get(<<"battlemap_id">>, JSONReqMap),
   io:format("~nLoading Battlemap ~p...~n", [BattlemapID]),
   {Battlemap, CharList} =
      timed_cache_object:fetch(
         battlemaps_db,
         BattlemapID
      ),
%%   ok = users_manager:ping(UserToken),
   jiffy:encode(
      [
         [
            <<"set_map">>,
            battlemap_battlemap:encode_in_json(Battlemap)
         ]
         |
         lists:map(
            fun (Char) ->
               [
                  <<"add_char">>,
                  battlemap_character:encode_in_json(Char)
               ]
            end,
            CharList
         )
      ]
   ).
