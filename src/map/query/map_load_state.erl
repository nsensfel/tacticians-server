-module(btl_load_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: btl_player:id(),
      session_token :: binary(),
      battle_id :: binary()
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
   BattleID = maps:get(<<"bid">>, JSONReqMap),

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

   Battle = shr_timed_cache:fetch(battle_db, PlayerID, BattleID),

   #query_state
   {
      battle = Battle
   }.

-spec generate_reply(query_state(), input()) -> binary().
generate_reply (QueryState, Input) ->
   PlayerID = Input#input.player_id,
   Battle = QueryState#query_state.battle,
   Players = btl_battle:get_players(Battle),

   PlayerIX =
      shr_array_util:first
      (
         fun (Player) ->
            (btl_player:get_id(Player) == PlayerID)
         end,
         Players
      ),

   true = (PlayerIX >= 0),

   SetTimeline =
      btl_set_timeline:generate
      (
         btl_battle:get_encoded_last_turns_effects(Battle)
      ),

   SetMap = btl_set_map:generate(btl_battle:get_battlemap(Battle)),

   AddCharList =
      array:sparse_to_list
      (
         array:map
         (
            fun (IX, Character) ->
               btl_add_char:generate(IX, Character, PlayerIX)
            end,
            btl_battle:get_characters(Battle)
         )
      ),

   AddWeaponList =
      lists:map
      (
         fun (WeaponID) ->
            btl_add_weapon:generate(shr_weapon:from_id(WeaponID))
         end,
         btl_battle:get_used_weapon_ids(Battle)
      ),

   AddArmorList =
      lists:map
      (
         fun (ArmorID) ->
            btl_add_armor:generate(shr_armor:from_id(ArmorID))
         end,
         btl_battle:get_used_armor_ids(Battle)
      ),

   AddTileList =
      lists:map
      (
         fun (TileID) ->
            btl_add_tile:generate(btl_tile:from_id(TileID))
         end,
         btl_battle:get_used_tile_ids(Battle)
      ),

   OutputList =
      (
         AddTileList
         ++ [SetTimeline, SetMap | AddWeaponList]
         ++ AddArmorList
         ++ AddCharList
      ),
   Output = jiffy:encode(OutputList),

   Output.

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   btl_security:assert_identity
   (
      Input#input.player_id,
      Input#input.session_token
   ),
   btl_security:lock_queries(Input#input.player_id),
   QueryState = fetch_data(Input),
   btl_security:unlock_queries(Input#input.player_id),
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
