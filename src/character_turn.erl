-module(character_turn).

-export(
   [
      handle_request/1
   ]
).

handle_request (Req) ->
   JSON_Req_Map = jiffy:decode(Req, [return_maps]),
   UserToken = maps:get(<<"user_token">>, JSON_Req_Map),
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
