-module(battlemap_shim).
-export
(
   [
      generate/0
   ]
).

-include("timed_cache_data.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate(_Prev, Result, _X, 0, _BaseWidth) ->
   Result;
generate(Prev, Result, 0, Y, BaseWidth) ->
   generate(Prev, Result, BaseWidth, (Y - 1), BaseWidth);
generate(Prev, Result, X, Y, BaseWidth) ->
   case rand:uniform(64) of
      N when (N >= 16) ->
         generate(Prev, [Prev|Result], (X - 1), Y, BaseWidth);

      N ->
         NewTileType = (N - 1),
         generate
         (
            NewTileType,
            [NewTileType|Result],
            (X - 1),
            Y,
            BaseWidth
         )
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate () ->
   Width = (rand:uniform(54) + 10),
   Height = (rand:uniform(54) + 10),
   #battlemap
   {
      id = <<"0">>,
      width = Width,
      height = Height,
      content = array:from_list(generate(0, [], Width, Height, Width))
   }.
