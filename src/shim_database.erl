-module(shim_database).
-export([fetch/2]).

fetch(battlemaps_db, _Object_ID) ->
   Width = (rand:uniform(54) + 10),
   Height = (rand:uniform(54) + 10),
   {ok,
      {
         shim_battlemap_battlemap:generate(Width, Height),
         shim_battlemap_character:generate(Width, Height)
      }
   }.
