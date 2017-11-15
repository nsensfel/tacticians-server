-module(shim_battlemap_character).

-export(
   [
      generate/2
   ]
).

generate_char (N, X, Y, Team) ->
   IDAsString = list_to_binary(integer_to_list(N)),
   {
      IDAsString, % ID
      IDAsString, % Name
      IDAsString, % Icon
      IDAsString, % Portrait
      {X, Y},
      Team,
      rand:uniform(10), % Movement Points
      (rand:uniform(5) - 1) % Attack Range
   }.

generate (0, Result, _MaxX, _MaxY) ->
   Result;
generate (N, Prev, MaxX, MaxY) ->
   generate
   (
      (N - 1),
      [
         generate_char
         (
            N,
            (rand:uniform(MaxX) - 1),
            (rand:uniform(MaxY) - 1),
            (N rem 2)
         )
         | Prev
      ],
      MaxX,
      MaxY
   ).

generate (MaxX, MaxY) ->
   generate(rand:uniform(14) + 2, [], MaxX, MaxY).
