-module(rst_load).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: binary(),
      session_token :: binary()
   }
).

-record
(
   query_state,
   {
      player :: shr_player:type(),
      inventory :: shr_inventory:type(),
      roster :: rst_roster:type()
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

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken
   }.

-spec authenticate_user (input()) -> ({'ok', shr_player:type()} | 'error').
authenticate_user (Input) ->
   PlayerID = Input#input.player_id,
   SessionToken = Input#input.session_token,

   Player = shr_timed_cache:fetch(player_db, any, PlayerID),

   case shr_security:credentials_match(SessionToken, Player) of
      true -> {ok, Player};
      _ -> error
   end.

-spec fetch_data (shr_player:type(), input()) -> query_state().
fetch_data (Player, Input) ->
   PlayerID = Input#input.player_id,
   RosterID = shr_player:get_roster_id(Player),
   InventoryID = shr_player:get_inventory_id(Player),

   Roster = shr_timed_cache:fetch(char_roster_db, PlayerID, RosterID),
   Inventory = shr_timed_cache:fetch(char_roster_db, PlayerID, InventoryID),

   #query_state
   {
      player = Player,
      roster = Roster,
      inventory = Inventory
   }.

-spec generate_reply(query_state()) -> binary().
generate_reply (QueryState) ->
   Roster = QueryState#query_state.roster,
   Inventory = QueryState#query_state.inventory,

   RosterCharacters = rst_roster:get_characters(Roster),
   SetInventory = shr_set_inventory:generate(Inventory),
   EncodedRoster =
      array:to_list
      (
         array:sparse_map(fun rst_add_char:generate/2, RosterCharacters)
      ),

   Output = jiffy:encode([SetInventory|EncodedRoster]),

   Output.

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   case authenticate_user(Input) of
      {ok, Player} ->
         shr_security:lock_queries(Input#input.player_id),
         QueryState = fetch_data(Player, Input),
         shr_security:unlock_queries(Input#input.player_id),
         generate_reply(QueryState);

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