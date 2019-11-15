-module(shr_roll).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      percentage/0,
      between/2,
      percentage_with_luck/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec calculate_costs (boolean(), 0..100, 0..100) -> integer().
calculate_costs (true, Roll, Chance) when (Chance > 50) ->
   % Succeeded, was likely to succeed.
   % Only pay what was used.
   max(0, (Roll - Chance));
calculate_costs (true, _Roll, Chance) when (Chance =< 50) ->
   % Succeeded, was unlikely to succeed.
   % Pay a lot!
   (Chance - 55);
calculate_costs (false, _Roll, Chance) when (Chance > 50) ->
   % Failure due to bad roll.
   % Was likely to succeed, you get a lot!
   (Chance - 45);
calculate_costs (_, _, _) ->
   % Failure on unlikely roll. Not costs.
   0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec between (non_neg_integer(), non_neg_integer()) -> non_neg_integer().
between (Min, Max) ->
   Diff = (Max - Min),
   (Min + (rand:uniform(Diff + 1) - 1)).

-spec percentage () -> 1..100.
percentage () ->
   between(1, 100).

-spec percentage_with_luck
   (
      non_neg_integer(),
      integer()
   )
   -> {0..100, boolean(), integer(), integer()}.
percentage_with_luck (Chance, Luck) ->
   Roll = percentage(),
   ModedChance = max(0, min((Chance + Luck), 100)),
   ModedRoll = max(0, min((Roll - Luck), 100)),
   IsSuccess = (Roll =< ModedChance),

   {
      ModedRoll,
      IsSuccess,
      calculate_costs(IsSuccess, Roll, Chance),
      calculate_costs((not IsSuccess), (100 - Roll), (100 - ModedChance))
   }.
