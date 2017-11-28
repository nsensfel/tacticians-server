-module(battlemap).
-record
(
   battlemap,
   {
      id,
      width,
      height,
      content,
      instances
   }
).
-export
(
   [
      get_id/1,
      get_width/1,
      get_height/1,
      list_tiles/1,
      get_instances/1
   ]
).
-export
(
   [dist/2]
).
-export([cross/5]).

get_id (Battlemap) -> Battlemap#battlemap.id.
get_width (Battlemap) -> Battlemap#battlemap.width.
get_height (Battlemap) -> Battlemap#battlemap.height.
list_tiles (Battlemap) -> array:sparse_to_list(Battlemap#battlemap.content).
get_instances (Battlemap) -> Battlemap#battlemap.instances.

-include("battlemap/cross.erl").

dist ({OX, OY}, {DX, DY}) ->
   (abs(OX - DX) + abs(OY + DY)).
