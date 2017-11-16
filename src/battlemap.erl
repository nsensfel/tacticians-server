-module(battlemap).
-export([encode_in_json/1]).

encode_in_json (
   {
      Width,
      Height,
      Tiles
   }
) ->
   jiffy:encode(
      {
         [
            {<<"width">>, Width},
            {<<"height">>, Height},
            {
               <<"content">>,
               lists:map((fun ({ID, Cost}) -> [ID, Cost] end), Tiles)
            }
         ]
      }
   ).
