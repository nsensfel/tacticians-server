-module(bm_player_turn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   player_turn,
   {
      number :: non_neg_integer(),
      player_ix :: non_neg_integer()
   }
).

-opaque type() :: #player_turn{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      new/2,
      next/2
   ]
).

%%%% Accessors
-export
(
   [
      get_number/1,
      get_player_ix/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec new (non_neg_integer(), non_neg_integer()) -> type().
new (Number, PlayerIX) ->
   #player_turn
   {
      number = Number,
      player_ix = PlayerIX
   }.

-spec get_number (type()) -> non_neg_integer().
get_number (PlayerTurn) -> PlayerTurn#player_turn.number.

-spec get_player_ix (type()) -> non_neg_integer().
get_player_ix (PlayerTurn) -> PlayerTurn#player_turn.player_ix.

-spec next (non_neg_integer(), type()) -> type().
next (PlayersCount, CurrentPlayerTurn) ->
   CurrentPlayerIX = CurrentPlayerTurn#player_turn.player_ix,
   CurrentTurnNumber = CurrentPlayerTurn#player_turn.number,

   NextPlayerIX = ((CurrentPlayerIX + 1) rem PlayersCount),
   NextTurnNumber =
      case NextPlayerIX of
         0 -> (CurrentTurnNumber + 1);
         _ -> CurrentTurnNumber
      end,

   new(NextTurnNumber, NextPlayerIX).
