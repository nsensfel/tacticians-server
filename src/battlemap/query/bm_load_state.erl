-module(bm_load_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: bm_player:id(),
      session_token :: binary(),
      battle_id :: binary()
   }
).

-record
(
   query_state,
   {
      battle :: bm_battle:type()
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
   BattleID = maps:get(<<"bmi">>, JSONReqMap),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      battle_id = BattleID
   }.

-spec fetch_data (input()) -> query_state().
fetch_data (Input) ->
   PlayerID = Input#input.player_id,
   BattleID = Input#input.battle_id,

   Battle = sh_timed_cache:fetch(battle_db, PlayerID, BattleID),

   #query_state
   {
      battle = Battle
   }.

-spec generate_reply(query_state(), input()) -> binary().
generate_reply (QueryState, Input) ->
   PlayerID = Input#input.player_id,
   Battle = QueryState#query_state.battle,
   SetTimeline =
      bm_set_timeline:generate
      (
         bm_battle:get_encoded_last_turns_effects(Battle)
      ),

   SetMap = bm_set_map:generate(bm_battle:get_battlemap(Battle)),

   AddCharList =
      array:sparse_to_list
      (
         array:map
         (
            fun (IX, Character) ->
               bm_add_char:generate(IX, Character, PlayerID)
            end,
            bm_battle:get_characters(Battle)
         )
      ),

   AddWeaponList =
      lists:map
      (
         fun (WeaponID) ->
            bm_add_weapon:generate(sh_weapon:from_id(WeaponID))
         end,
         bm_battle:get_used_weapon_ids(Battle)
      ),

   AddArmorList =
      lists:map
      (
         fun (ArmorID) ->
            bm_add_armor:generate(sh_armor:from_id(ArmorID))
         end,
         bm_battle:get_used_armor_ids(Battle)
      ),

   jiffy:encode
   (
      [SetTimeline, SetMap | AddWeaponList]
      ++ AddArmorList
      ++ AddCharList
   ).

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   bm_security:assert_identity
   (
      Input#input.player_id,
      Input#input.session_token
   ),
   bm_security:lock_queries(Input#input.player_id),
   QueryState = fetch_data(Input),
   bm_security:unlock_queries(Input#input.player_id),
   generate_reply(QueryState, Input).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
