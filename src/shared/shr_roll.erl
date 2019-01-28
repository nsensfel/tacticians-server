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
      percentage_with_luck/2,
      conflict_with_luck/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec between (non_neg_integer(), non_neg_integer()) -> non_neg_integer().
between (Min, Max) ->
   Diff = (Max - Min),
   (Min + (rand:uniform(Diff + 1) - 1)).

-spec percentage () -> 0..100.
percentage () ->
   between(0, 100).

-spec percentage_with_luck
   (
      non_neg_integer(),
      integer()
   )
   -> {0..100, boolean(), integer()}.
percentage_with_luck (Target, Luck) ->
   BaseRoll = percentage(),
   ModedRoll = max(0, min((BaseRoll - Luck), 100)),
   IsSuccess = (ModedRoll =< Target),

   NewLuck =
      case {IsSuccess, (Target > 50)} of
         {true, true} ->
            % Succeeded, was likely to succeed.
            % Only pay what was used.
            MissingPoints = max(0, (BaseRoll - Target)),
            (Luck - MissingPoints);

         {true, false} ->
            % Succeeded, was unlikely to succeed.
            % Pay a lot!
            MissingPoints = (55 - Target),
            (Luck - MissingPoints);

         {false, true} ->
            % Failure due to bad roll.
            % Was likely to succeed, you get a lot!
            OwedPoints = (Target - 45),
            (Luck + OwedPoints);

         _ -> Luck
      end,

   {ModedRoll, IsSuccess, NewLuck}.

-spec conflict_with_luck
   (
      non_neg_integer(),
      integer(),
      integer()
   )
   -> {0..100, boolean(), integer(), integer()}.
conflict_with_luck (Target, LuckA, LuckB) ->
   BaseRoll = percentage(),
   ModedRoll = max(0, min((BaseRoll - (LuckA - LuckB)), 100)),
   IsSuccess = (ModedRoll =< Target),

   {NewLuckA, NewLuckB} =
      case {IsSuccess, (Target > 50)} of
         {true, true} ->
            % Succeeded, was likely to succeed.
            % Only pay what was used.
            MissingPoints = max(0, (BaseRoll - Target)),
            {(LuckA - MissingPoints), LuckB};

         {true, false} ->
            % Succeeded, was unlikely to succeed.
            % Pay a lot!
            MissingPoints = (55 - Target),
            {(LuckA - MissingPoints), (LuckB + MissingPoints)};

         {false, true} ->
            % Failure due to bad roll.
            % Was likely to succeed, you get a lot!
            OwedPoints = (Target - 45),
            {(LuckA + OwedPoints), (LuckB - OwedPoints)};

         _ -> {LuckA, LuckB}
      end,

   {ModedRoll, IsSuccess, NewLuckA, NewLuckB}.
