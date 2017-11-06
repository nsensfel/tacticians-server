-module(character_turn).

-export(
   [
      handle/1
   ]
).

handle (Req) ->
   io:format("~nReceived~p...", [Req]),
   JSON_Req_Map = jiffy:decode(Req, [return_maps]),
   UserToken = maps:get(<<"user_token">>, JSON_Req_Map),
   io:format("~nCharacter Turn for ~p...", [UserToken]),
%%   ok = users_manager:ping(UserToken),
   jiffy:encode(
      {
         [
            {<<"types">>, [<<"STATUS">>]},
            {
               <<"data">>,
               [
                  {
                     [
                        {
                           <<"status">>,
                           <<"OK">>
                        }
                     ]
                  }
               ]
            }
         ]
      }
   ).
