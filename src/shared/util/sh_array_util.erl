-module(sh_array_util).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      any/2,
      any_indexed/2,
      none/2,
      all/2,

      first/2,

      mapiff/3
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec any_internals
   (
      fun((any()) -> boolean()),
      array:array(any()),
      non_neg_integer()
   )
   -> boolean().
any_internals (_, _, 0) ->
   false;
any_internals (Fun, Array, PrevIX) ->
   IX = (PrevIX - 1),
   case Fun(array:get(IX, Array)) of
      true -> true;
      _ -> any_internals(Fun, Array, IX)
   end.

-spec first_internals
   (
      fun((any()) -> boolean()),
      array:array(any()),
      non_neg_integer()
   )
   -> integer().
first_internals (_, _, 0) ->
   -1;
first_internals (Fun, Array, PrevIX) ->
   IX = (PrevIX - 1),
   case Fun(array:get(IX, Array)) of
      true -> IX;
      _ -> first_internals(Fun, Array, IX)
   end.

-spec any_indexed_internals
   (
      fun((non_neg_integer(), any()) -> boolean()),
      array:array(any()),
      non_neg_integer()
   )
   -> boolean().
any_indexed_internals (_, _, 0) ->
   false;
any_indexed_internals (Fun, Array, PrevIX) ->
   IX = (PrevIX - 1),
   case Fun(IX, array:get(IX, Array)) of
      true -> true;
      _ -> any_indexed_internals(Fun, Array, IX)
   end.

-spec all_internals
   (
      fun((any()) -> boolean()),
      array:array(any()),
      non_neg_integer()
   )
   -> boolean().
all_internals (_, _, 0) ->
   true;
all_internals (Fun, Array, PrevIX) ->
   IX = (PrevIX - 1),
   case Fun(array:get(IX, Array)) of
      true -> any_internals(Fun, Array, IX);
      _ -> false
   end.

-spec mapiff_internals
   (
      fun((any()) -> boolean()),
      fun((any()) -> any()),
      array:array(any()),
      list(non_neg_integer()),
      non_neg_integer()
   )
   -> {array:array(any()), list(non_neg_integer())}.
mapiff_internals (_, _, Array, IXList, 0) ->
   {Array, IXList};
mapiff_internals (Cond, Map, Array, IXList, PrevIX) ->
   IX = (PrevIX - 1),
   Elem = array:get(IX, Array),

   case Cond(Elem) of
      false -> mapiff_internals(Cond, Map, Array, IXList, IX);
      _ ->
         mapiff_internals
         (
            Cond,
            Map,
            array:set(IX, Map(Elem), Array),
            [IX|IXList],
            IX
         )
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec any (fun((any()) -> boolean()), array:array(any())) -> boolean().
any (Fun, Array) ->
   any_internals(Fun, Array, array:size(Array)).

-spec first (fun((any()) -> boolean()), array:array(any())) -> integer().
first (Fun, Array) ->
   first_internals(Fun, Array, array:size(Array)).

-spec any_indexed
   (
      fun((non_neg_integer(), any()) -> boolean()),
      array:array(any())
   ) -> boolean().
any_indexed (Fun, Array) ->
   any_indexed_internals(Fun, Array, array:size(Array)).

-spec all (fun((any()) -> boolean()), array:array(any())) -> boolean().
all (Fun, Array) ->
   all_internals(Fun, Array, array:size(Array)).

-spec none (fun((any()) -> boolean()), array:array(any())) -> boolean().
none (Fun, Array) ->
   not any(Fun, Array).

-spec mapiff
   (
      fun((any()) -> boolean()),
      fun((any()) -> any()),
      array:array(any())
   )
   -> {array:array(any()), list(non_neg_integer())}.
mapiff (Cond, Map, Array) ->
   mapiff_internals(Cond, Map, Array, [], array:size(Array)).
