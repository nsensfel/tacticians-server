-module(tile).
-export
(
   [
      get_cost/1,
      cost_when_oob/0
   ]
).
cost_when_oob () -> 255.

get_cost (N) ->
   if
      (N =< 200) -> (N + 1);
      true -> cost_when_oob()
   end.
