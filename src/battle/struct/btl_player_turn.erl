-module(btl_player_turn).

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

-spec next_valid_player
   (
      non_neg_integer(),
      orddict:orddict(non_neg_integer(), btl_player:type()),
      non_neg_integer(),
      non_neg_integer()
   ) -> non_neg_integer().
next_valid_player (StartingPoint, _Players, _PlayersCount, StartingPoint) ->
   StartingPoint;
next_valid_player (CandidateIX, Players, PlayersCount, StartingPoint) ->
   Candidate = orddict:fetch(CandidateIX, Players),

   case btl_player:get_is_active(Candidate) of
      true -> CandidateIX;
      _ ->
         next_valid_player
         (
            ((CandidateIX + 1) rem PlayersCount),
            Players,
            PlayersCount,
            StartingPoint
         )
   end.

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

-spec next
   (
      orddict:orddict(non_neg_integer(), btl_player:type()),
      type()
   )
   -> type().
next (Players, CurrentPlayerTurn) ->
   CurrentPlayerIX = CurrentPlayerTurn#player_turn.player_ix,
   CurrentTurnNumber = CurrentPlayerTurn#player_turn.number,
   PlayersCount = orddict:size(Players),

   NextPlayerIX =
      next_valid_player
      (
         ((CurrentPlayerIX + 1) rem PlayersCount),
         Players,
         PlayersCount,
         CurrentPlayerIX
      ),

   NextTurnNumber =
      case (NextPlayerIX < CurrentPlayerIX) of
         true -> (CurrentTurnNumber + 1);
         _ -> CurrentTurnNumber
      end,

   new(NextTurnNumber, NextPlayerIX).
