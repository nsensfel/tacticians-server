-module(rst_load).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
-spec parse_input (shr_query:type()) -> input().
parse_input (Query) ->
   JSONReqMap = shr_query:get_params(Query),
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

   Player = shr_timed_cache:fetch(player_db, ataxia_security:any(), PlayerID),

   case shr_security:credentials_match(SessionToken, Player) of
      true -> {ok, Player};
      _ -> error
   end.

-spec fetch_data (shr_player:type(), input()) -> query_state().
fetch_data (Player, Input) ->
   PlayerID = Input#input.player_id,
   RosterID = shr_player:get_roster_id(Player),
   % InventoryID = shr_player:get_inventory_id(Player),

   Roster =
      shr_timed_cache:fetch
      (
         roster_db,
         ataxia_security:user_from_id(PlayerID),
         RosterID
      ),
   %% TODO
   %% Inventory = shr_timed_cache:fetch(inventory_db, PlayerID, InventoryID),

   io:format("[W] Using shim inventory.~n"),

   Inventory = shr_inventory:new(PlayerID),

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
      lists:map
      (
         fun ({IX, Char}) ->
            rst_add_char:generate(IX, Char)
         end,
         orddict:to_list(RosterCharacters)
      ),

   Output = jiffy:encode([SetInventory|EncodedRoster]),

   Output.

-spec handle (shr_query:type()) -> binary().
handle (Query) ->
   Input = parse_input(Query),
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
      handle(shr_query:new(A))
   }.
