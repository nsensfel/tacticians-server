-module(shim_battlemap_battlemap).

-export(
   [
      generate/2
   ]
).

generate_char (N, X, Y, Team, Mov, Atk) ->
   IDAsString = integer_to_list(N),
   {IDAsString, IDAsString, IDAsString, IDAsString, {X, Y}, Team, Mov, Atk}.
