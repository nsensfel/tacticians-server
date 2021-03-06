-module(blc_distribution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      generate/2,
      generate/3
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_internals
   (
      non_neg_integer(),
      list(list(0..100)),
      list(0..100)
   )
   -> list(list(0..100)).
generate_internals (0, CurrentResult, _Sequence) ->
   lists:filter(fun (E) -> (lists:sum(E) == 100) end, CurrentResult);
generate_internals (N, CurrentResult, Sequence) ->
   generate_internals
   (
      (N - 1),
      lists:filter
      (
         fun (E) -> (lists:sum(E) =< 100) end,
         shr_lists_util:product
         (
            fun (L, E) ->
               [E|L]
            end,
            CurrentResult,
            Sequence
         )
      ),
      Sequence
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate (non_neg_integer(), 0..100) -> list(list(0..100)).
generate (0, _Step) -> [];
generate (Elements, Step) ->
   generate(Elements, 0, Step).

-spec generate (non_neg_integer(), 0..100, 0..100) -> list(list(0..100)).
generate (0, _Min, _Step) -> [];
generate (Elements, Min, Step) ->
   Sequence = lists:seq(Min, 100, Step),
   generate_internals
   (
      (Elements - 1),
      lists:map(fun (E) -> [E] end, Sequence),
      Sequence
   ).
