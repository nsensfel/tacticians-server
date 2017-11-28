-module(battlemap).
-export([dist/2]).
-include("timed_cache_data.hrl").

-include("battlemap/cross.erl").

dist ({OX, OY}, {DX, DY}) ->
   (abs(OX - DX) + abs(OY + DY)).
