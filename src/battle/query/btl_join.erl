-module(btl_join).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-type mode() :: (attack | defend | {invalid, binary()}).

-record
(
   input,
   {
      player_id :: shr_player:id(),
      session_token :: binary(),
      mode :: mode(),
      size :: non_neg_integer(),
      roster_ixs :: list(non_neg_integer()),
      map_id :: string()
   }
).

-record
(
   query_state,
   {
      battle :: btl_battle:type()
   }
).

-type input() :: #input{}.
-type query_state() :: #query_state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec parse_input (binary()) -> input().
parse_input (Req) ->
   JSONReqMap = jiffy:decode(Req, [return_maps]),
   PlayerID = maps:get(<<"pid">>, JSONReqMap),
   SessionToken =  maps:get(<<"stk">>, JSONReqMap),

   Mode =
      case maps:get(<<"m">>, JSONReqMap) of
         <<"a">> -> attack;
         <<"b">> -> defend;
         V -> {invalid, V}
      end,

   true = ((Mode == attack) or (Mode == defend)),

   Size =
      case maps:get(<<"s">>, JSONReqMap) of
         <<"s">> -> 8;
         <<"m">> -> 16;
         <<"l">> -> 24;
         _ -> 0
      end,

   Roster = maps:get(<<"r">>, JSONReqMap),
   MapID =  maps:get(<<"map_id">>, JSONReqMap),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      mode = Mode,
      size = Size,
      roster_ixs = Roster,
      map_id = MapID
   }.

-spec authenticate_user (input()) -> ('ok' | 'error').
authenticate_user (Input) ->
   PlayerID = Input#input.player_id,
   SessionToken = Input#input.session_token,

   Player = shr_timed_cache:fetch(player_db, any, PlayerID),

   case shr_security:credentials_match(SessionToken, Player) of
      true -> ok;
      _ -> error
   end.

-spec fetch_data (input()) -> query_state().
fetch_data (Input) ->
   PlayerID = Input#input.player_id,
   BattleID = Input#input.battle_id,

   Battle = shr_timed_cache:fetch(battle_db, PlayerID, BattleID),

   #query_state
   {
      battle = Battle
   }.


-spec generate_reply(query_state(), input()) -> binary().
generate_reply (QueryState, Input) ->

   Output.

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   case authenticate_user(Input) of
      ok ->
         shr_security:lock_queries(Input#input.player_id),
         QueryState = fetch_data(Input),
         shr_security:unlock_queries(Input#input.player_id),
         generate_reply(QueryState, Input);

      error -> jiffy:encode([shr_disconnected:generate()])
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
