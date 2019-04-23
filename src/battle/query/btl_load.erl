-module(btl_load).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   input,
   {
      player_id :: shr_player:id(),
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
-spec parse_input (shr_query:type()) -> input().
parse_input (Query) ->
   JSONReqMap = shr_query:get_params(Query),

   PlayerID = maps:get(<<"pid">>, JSONReqMap),
   SessionToken =  maps:get(<<"stk">>, JSONReqMap),
   BattleID = maps:get(<<"bid">>, JSONReqMap),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      battle_id = BattleID
   }.

-spec authenticate_user (input()) -> ('ok' | 'error').
authenticate_user (Input) ->
   PlayerID = Input#input.player_id,
   SessionToken = Input#input.session_token,

   Player = shr_timed_cache:fetch(player_db, ataxia_security:any(), PlayerID),

   case shr_security:credentials_match(SessionToken, Player) of
      true -> ok;
      _ -> error
   end.

-spec fetch_data (input()) -> query_state().
fetch_data (Input) ->
   PlayerID = Input#input.player_id,
   BattleID = Input#input.battle_id,

   Battle =
      shr_timed_cache:fetch
      (
         battle_db,
         ataxia_security:user_from_id(PlayerID),
         BattleID
      ),

   #query_state
   {
      battle = Battle
   }.


-spec generate_reply(query_state(), input()) -> binary().
generate_reply (QueryState, Input) ->
   PlayerID = Input#input.player_id,
   PUser = ataxia_security:user_from_id(PlayerID),
   Battle = QueryState#query_state.battle,
   RelevantInventory = btl_battle:get_relevant_inventory(Battle),
   Players = btl_battle:get_players(Battle),

   {value, {PlayerIX, _Player}} =
      shr_lists_util:search
      (
         fun ({_PlayerIX, Player}) ->
            (btl_player:get_id(Player) == PlayerID)
         end,
         orddict:to_list(Players)
      ),

   true = (PlayerIX >= 0),

   SetTimeline =
      btl_set_timeline:generate
      (
         btl_battle:get_encoded_last_turns_effects(Battle)
      ),

   SetMap =
      shr_set_map:generate
      (
         PUser,
         fun (_TriggerName) -> false end,
         btl_battle:get_map(Battle)
      ),

   AddCharList =
      lists:map
      (
         fun ({IX, Character}) ->
            btl_add_char:generate(IX, Character, PlayerIX)
         end,
         orddict:to_list(btl_battle:get_characters(Battle))
      ),

   AddPlayerList =
      lists:map
      (
         fun ({IX, Player}) ->
            btl_add_player:generate(IX, Player)
         end,
         orddict:to_list(btl_battle:get_players(Battle))
      ),

   AddPortraitList =
      lists:map
      (
         fun (PortraitID) ->
            btl_add_portrait:generate(shr_portrait:from_id(PortraitID))
         end,
         ordsets:to_list(shr_inventory:get_portraits(RelevantInventory))
      ),

   AddWeaponList =
      lists:map
      (
         fun (WeaponID) ->
            btl_add_weapon:generate(shr_weapon:from_id(WeaponID))
         end,
         ordsets:to_list(shr_inventory:get_weapons(RelevantInventory))
      ),

   AddArmorList =
      lists:map
      (
         fun (ArmorID) ->
            btl_add_armor:generate(shr_armor:from_id(ArmorID))
         end,
         ordsets:to_list(shr_inventory:get_armors(RelevantInventory))
      ),

   AddGlyphList =
      lists:map
      (
         fun (GlyphID) ->
            btl_add_glyph:generate(shr_glyph:from_id(GlyphID))
         end,
         ordsets:to_list(shr_inventory:get_glyphs(RelevantInventory))
      ),

   AddGlyphBoardList =
      lists:map
      (
         fun (GlyphBoardID) ->
            btl_add_glyph_board:generate(shr_glyph_board:from_id(GlyphBoardID))
         end,
         ordsets:to_list(shr_inventory:get_glyph_boards(RelevantInventory))
      ),

   AddTileList =
      lists:map
      (
         fun (TileClassID) ->
            btl_add_tile:generate(shr_tile:from_id(TileClassID))
         end,
         ordsets:to_list(btl_battle:get_related_tile_ids(Battle))
      ),

   OutputList =
      (
         AddTileList
         ++ [SetTimeline, SetMap | AddWeaponList]
         ++ AddPortraitList
         ++ AddGlyphList
         ++ AddGlyphBoardList
         ++ AddArmorList
         ++ AddPlayerList
         ++ AddCharList
      ),

   Output = jiffy:encode(OutputList),

   Output.

-spec handle (shr_query:type()) -> binary().
handle (Query) ->
   Input = parse_input(Query),
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
      handle(shr_query:new(A))
   }.
