-module(database_shim).
-export([fetch/2]).

-include("timed_cache_data.hrl").

fetch(battlemap_db, Object_ID) ->
   Width = (rand:uniform(54) + 10),
   Height = (rand:uniform(54) + 10),
   io:format
   (
      "~nGenerating new Battlemap ~p of size (~p, ~p)...~n",
      [Object_ID, Width, Height]
   ),
   Result =
      #battlemap {
         id = Object_ID,
         width = Width,
         height = Height,
         content = battlemap_shim:generate(Width, Height)
      },
   {ok,
      {
         character_shim:generate(Width, Height)
      }
   };
fetch(battlemap_db, Object_ID) ->
