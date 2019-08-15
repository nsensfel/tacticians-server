-module(blc_damage_type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type entry() :: {shr_damage_type:type(), non_neg_integer()}.
-type coefficient() :: {shr_damage_type:type(), 0..100}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([entry/0, coefficient/0]).

-export
(
   [
      sort_entries/1,
      compute_score/1,
      apply_score_modifier/3,
      generate_entries_from_score/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sort_entries (list(entry())) -> list(entry()).
sort_entries (Entries) ->
   lists:sort
   (
      fun ({_NameA, ValueA}, {_NameB, ValueB}) -> (ValueA >= ValueB) end,
      Entries
   ).

-spec compute_score (list(entry())) -> non_neg_integer().
compute_score (SortedEntries) ->
   {_LastIndex, Result} =
      lists:foldl
      (
         fun ({_Name, Value}, {Index, Current}) ->
            {(Index + 1), (Current + (Index * Value))}
         end,
         {1, 0},
         SortedEntries
      ),

   Result.

-spec apply_score_modifier
   (
      non_neg_integer(),
      (-1 | 1),
      list(entry())
   )
   -> list(entry()).
apply_score_modifier (AbsModifier, Mod, S0SortedEntries) ->
   {S1SortedEntries, {_EndIndex, EndModifier}} =
      lists:mapfoldl
      (
         fun ({Name, S0Value}, {Index, RemainingModifier}) ->
            case ((RemainingModifier >= Index) and (S0Value > 0)) of
               true ->
                  {
                     {Name, (S0Value + Mod)},
                     {(Index + 1), (RemainingModifier - Index)}
                  };

               false -> {{Name, S0Value}, {(Index + 1), RemainingModifier}}
            end
         end,
         {1, AbsModifier},
         S0SortedEntries
      ),

   case (EndModifier > 0) of
      false -> S1SortedEntries;
      true -> apply_score_modifier(EndModifier, Mod, S1SortedEntries)
   end.

-spec generate_entries_from_score
   (
      non_neg_integer(),
      list(coefficient())
   )
   -> list(entry()).
generate_entries_from_score (TargetScore, SortedRatios) ->
   {Distribution, _LastIndex} =
      lists:foldl
      (
         fun ({_Name, Value}, {Cumul, Index}) ->
            {(Cumul + (Value * Index)), (Index + 1)}
         end,
         {0, 1},
         SortedRatios
      ),

   Base = (TargetScore / (Distribution / 100)),

   UnderperformingEntries =
      lists:map
      (
         fun ({Name, Value}) -> {Name, trunc(Base * (Value / 100))} end,
         SortedRatios
      ),

   MissingScore = (TargetScore - compute_score(UnderperformingEntries)),

   case (MissingScore >= 0) of
      true -> apply_score_modifier(MissingScore, 1, UnderperformingEntries);
      false ->
         apply_score_modifier((-1 * MissingScore), -1, UnderperformingEntries)
   end.
