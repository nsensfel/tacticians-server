-module(location).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type type() :: {non_neg_integer(), non_neg_integer()}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      decode/1,
      encode/1
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
   true = (X >= 0),
   true = (Y >= 0),
   {X, Y}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_direction (direction:enum(), type()) -> type().
apply_direction (left, {X, Y}) ->
   validate({(X - 1), Y});
apply_direction (right, {X, Y}) ->
   validate({(X + 1), Y});
apply_direction (up, {X, Y}) ->
   validate({X, (Y - 1)});
apply_direction (down, {X, Y}) ->
   validate({X, (Y + 1)}).

-spec dist(type(), type()) -> non_neg_integer().
dist ({OX, OY}, {DX, DY}) ->
   (abs(DY - OY) + abs(DX - OX)).

-spec encode (type()) -> list(non_neg_integer()).
encode ({X, Y}) -> [X, Y].

-spec decode (list(non_neg_integer())) -> type().
decode ([X, Y]) when (is_integer(X) and is_integer(Y)) -> validate({X, Y}).
