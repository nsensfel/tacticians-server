-module(shim_battlemap_battlemap).

-export(
   [
      generate/2
   ]
).

generate_tile (0) ->
   {0, 1};
generate_tile (1) ->
   {1, 2};
generate_tile (2) ->
   {2, 99}.

generate(_Prev, Result, _X, 0, _BaseWidth) ->
   Result;
generate(Prev, Result, 0, Y, BaseWidth) ->
   generate(Prev, Result, BaseWidth, (Y - 1), BaseWidth);
generate(Prev, Result, X, Y, BaseWidth) ->
   case rand:uniform(6) of
      N when (N > 3) ->
         generate(Prev, [generate_tile(Prev)|Result], (X - 1), Y, BaseWidth);

      N ->
         NewTileType = (N - 1),
         generate(
            NewTileType,
            [generate_tile(NewTileType)|Result],
            (X - 1),
            Y,
            BaseWidth
         )
   end.

generate (Width, Height) ->
   {Width, Height, generate(0, [], Width, Height, Width)}.
