-module(shr_lists_util).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      %%% Gentoo hasn't marked Erlang/OTP 21 as stable yet, but I'd like to
      %%% use this function.
      %%% TODO: remove once lists:search/2 is available.
      search/2,
      product/3
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec product_internals
   (
      list(A),
      list(B),
      fun((A, B) -> C),
      list(C)
   )
   -> list(C).
product_internals ([], _ListB, _Fun, Result) ->
   Result;
product_internals ([A|Next], ListB, Fun, Result) ->
   product_internals
   (
      Next,
      ListB,
      Fun,
      (
         lists:map(fun (B) -> Fun(A, B) end, ListB)
         ++ Result
      )
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copy/pasted from the Erlang OTP's source code...
search (Pred, [Hd|Tail]) ->
   case Pred(Hd) of
      true -> {value, Hd};
      false -> search(Pred, Tail)
   end;
search (Pred, []) when is_function(Pred, 1) ->
   false.

-spec product (fun((A, B) -> C), list(A), list(B)) -> list(C).
product (Fun, ListA, ListB) ->
   product_internals(ListA, ListB, Fun, []).
