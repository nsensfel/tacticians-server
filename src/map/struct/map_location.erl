-module(map_location).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type type() :: ({non_neg_integer(), non_neg_integer()} | 'nowhere').

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      decode/1,
      encode/1,
      get_nowhere/0
   ]
).

-export
(
   [
      apply_direction/2,
      dist/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec validate ({integer(), integer()}) -> type().
validate ({X, Y}) ->
   if
      (X < 0) -> nowhere;
      (Y < 0) -> nowhere;
      true -> {X, Y}
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_nowhere () -> type().
get_nowhere () -> nowhere.

-spec apply_direction (map_direction:enum(), type()) -> type().
apply_direction (left, {X, Y}) ->
   validate({(X - 1), Y});
apply_direction (right, {X, Y}) ->
   validate({(X + 1), Y});
apply_direction (up, {X, Y}) ->
   validate({X, (Y - 1)});
apply_direction (down, {X, Y}) ->
   validate({X, (Y + 1)});
apply_direction (_, nowhere) ->
   error("Trying to move from 'nowhere'."),
   nowhere.

-spec dist(type(), type()) -> non_neg_integer().
dist ({OX, OY}, {DX, DY}) ->
   (abs(DY - OY) + abs(DX - OX));
dist (_, _) ->
   error("Trying to measure distance to 'nowhere'"),
   999.

-spec encode (type()) -> {list(any())}.
encode ({X, Y}) ->
   {
      [
         {<<"x">>, X},
         {<<"y">>, Y}
      ]
   };
encode (nowhere) ->
   {
      [
         {<<"x">>, -1},
         {<<"y">>, -1}
      ]
   }.

-spec decode (map()) -> type().
decode (Map) ->
   X = maps:get(<<"x">>, Map),
   Y = maps:get(<<"y">>, Map),

   true = (is_integer(X) and is_integer(Y)),

   validate({X, Y}).
