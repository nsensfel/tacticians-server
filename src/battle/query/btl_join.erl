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
      map_id :: ataxia_id:type()
   }
).


-type input() :: #input{}.
-type defend_query_state() :: 'ok'.
-type attack_query_state() :: 'ok'.

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
         <<"d">> -> defend;
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
   MapID = maps:get(<<"map_id">>, JSONReqMap),

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

-spec handle_attack (input()) -> 'ok'.
handle_attack (Input) ->
   PlayerID = Input#input.player_id,
   SelectedCharacterIXs = Input#input.roster_ixs,
   PlayerDBUser = ataxia_security:user_from_id(PlayerID),
   PartySize = length(SelectedCharacterIXs),

   % TODO: be less brutal if none is found.
   {ok, AvailablePendingBattle, AvailablePendingBattleID} =
      ataxia_client:update_and_fetch_any
      (
         pending_battle_db,
         PlayerDBUser,
         ataxic:update_lock
         (
            ataxic:apply_function
            (
               ataxia_lock,
               locked,
               [
                  ataxic:constant(PlayerDBUser),
                  ataxic:constant(60)
               ]
            )
         ),
         ataxic:ge
         (
            ataxic:field
            (
               ataxia_entry:get_value_field(),
               ataxic:field
               (
                  btl_pending_battle:get_free_slots_field(),
                  ataxic:current_value()
               )
            ),
            ataxic:constant(PartySize)
         )
         % missing: test that user isn't already a participant.
      ),

   bnt_join_battle:attempt
   (
      PlayerID,
      SelectedCharacterIXs,
      AvailablePendingBattleID,
      AvailablePendingBattle
   ),

   ok.
-spec handle_defend (input()) -> 'ok'.
handle_defend (Input) ->
   PlayerID = Input#input.player_id,
   SelectedCharacterIXs = Input#input.roster_ixs,
   MapID = Input#input.map_id,

   bnt_join_battle:generate(PlayerID, MapID, SelectedCharacterIXs),

   ok.

-spec fetch_attack_data (input()) -> attack_query_state().
fetch_attack_data (_Input) -> ok. % TODO

-spec fetch_defend_data (input()) -> defend_query_state().
fetch_defend_data (_Input) -> ok. % TODO

-spec authorize_attack (attack_query_state(), input()) -> 'ok'.
authorize_attack (_QueryState, _Input) -> ok. % TODO

-spec authorize_defend (defend_query_state(), input()) -> 'ok'.
authorize_defend (_QueryState, _Input) -> ok. % TODO

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   case authenticate_user(Input) of
      ok ->
         case Input#input.mode of
            attack ->
               QueryState = fetch_attack_data(Input),
               ok = authorize_attack(QueryState, Input),
               handle_attack(Input);

            defend ->
               QueryState = fetch_defend_data(Input),
               ok = authorize_defend(QueryState, Input),
               handle_defend(Input)
         end,
         jiffy:encode([shr_okay:generate()]);

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
