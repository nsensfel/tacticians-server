-module(shim_database).
-export([fetch/2]).

fetch(battlemaps_db, Object_ID) ->
   Width = (rand:uniform(54) + 10),
   Height = (rand:uniform(54) + 10),
   io:format
   (
      "~nGenerating new Battlemap ~p of size (~p, ~p)...~n",
      [Object_ID, Width, Height]
   ),
   {ok,
      {
         shim_battlemap_battlemap:generate(Width, Height),
         shim_battlemap_character:generate(Width, Height)
      }
   }.
