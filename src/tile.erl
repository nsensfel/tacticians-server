-module(tile).
-export
(
   [
      get_cost/1,
      cost_when_oob/0
   ]
).
cost_when_oob () -> 255.

get_cost(N) when (N =< 200) -> N;
get_cost(_N) -> cost_when_oob().
