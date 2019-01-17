-module(btl_join).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: shr_player:id(),
      session_token :: binary(),
      mode :: shr_battle_summary:mode(),
      category :: shr_battle_summary:category(),
      summary_ix :: non_neg_integer(),
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
   SummaryIX = maps:get(<<"six">>, JSONReqMap),

   Mode =
      case maps:get(<<"mod">>, JSONReqMap) of
         <<"a">> -> attack;
         <<"d">> -> defend;
         _ -> none
      end,

   Category =
      case maps:get(<<"cat">>, JSONReqMap) of
         <<"e">> -> event;
         <<"i">> -> invasion;
         _ -> quest
      end,

   Size =
      case maps:get(<<"s">>, JSONReqMap) of
         <<"s">> -> 8;
         <<"m">> -> 16;
         <<"l">> -> 24;
         _ -> 0
      end,

   true = (Size > 0),

   Roster = maps:get(<<"r">>, JSONReqMap),

   MapID = maps:get(<<"map_id">>, JSONReqMap),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      mode = Mode,
      category = Category,
      size = Size,
      summary_ix = SummaryIX,
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
   Mode = Input#input.mode,
   Category = Input#input.category,
   SummaryIX = Input#input.summary_ix,
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
         ataxic:land
         (
            [
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
               ),
               ataxic:neg
               (
                  ataxic:field
                  (
                     btl_pending_battle:get_player_ids_field(),
                     ataxic:apply_function
                     (
                        lists,
                        member,
                        [
                           ataxic:constant(PlayerID),
                           ataxic:current_value()
                        ]
                     )
                  )
               )
            ]
         )
      ),

   bnt_join_battle:attempt
   (
      PlayerID,
      Mode,
      Category,
      SummaryIX,
      SelectedCharacterIXs,
      AvailablePendingBattleID,
      AvailablePendingBattle
   ),

   ok.
-spec handle_defend (input()) -> 'ok'.
handle_defend (Input) ->
   PlayerID = Input#input.player_id,
   SelectedCharacterIXs = Input#input.roster_ixs,
   Mode = Input#input.mode,
   Category = Input#input.category,
   SummaryIX = Input#input.summary_ix,
   MapID = Input#input.map_id,

   bnt_join_battle:generate
   (
      PlayerID,
      Mode,
      Category,
      SummaryIX,
      MapID,
      SelectedCharacterIXs
   ),

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
