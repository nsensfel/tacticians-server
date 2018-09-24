-module(rst_update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: binary(),
      session_token :: binary(),
      character_ix :: non_neg_integer(),
      character :: rst_character:type()
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
   CharacterIX = maps:get(<<"cix">>, JSONReqMap),
   EncodedCharacter = maps:get(<<"chr">>, JSONReqMap),

   Character = rst_character:decode(EncodedCharacter),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      character_ix = CharacterIX,
      character = Character
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

   Roster = shr_timed_cache:fetch(roster_db, PlayerID, RosterID),
   Inventory = shr_timed_cache:fetch(roster_db, PlayerID, InventoryID),

   #query_state
   {
      player = Player,
      roster = Roster,
      inventory = Inventory
   }.

-spec update_data (query_state(), input()) -> query_state().
update_data (QueryState, Input) ->
   Inventory = QueryState#query_state.inventory,
   Character = Input#input.character,

   rst_character:validate(Inventory, Character),

   %% TODO [FUNCTION: chr][REQUIRED]: unimplemented.
   QueryState.

-spec commit_update (query_state(), input()) -> 'ok'.
commit_update (QueryState, Input) ->
   PlayerID = Input#input.player_id,
   CharacterIX = Input#input.character_ix,
   Character = Input#input.character,
   Player = QueryState#query_state.player,
   Roster = QueryState#query_state.roster,

   RosterID = shr_player:get_roster_id(Player),
   UpdatedRoster = rst_roster:set_character(CharacterIX, Character, Roster),

   Query =
      shr_db_query:new
      (
         roster_db,
         RosterID,
         {user, PlayerID},
         [
            shr_db_query:update_indexed
            (
               rst_roster:get_characters_field(),
               CharacterIX,
               [shr_db_query:set_value(Character)]
            )
         ]
      ),

   shr_database:commit(Query),
   shr_timed_cache:update(roster_db, PlayerID, RosterID, UpdatedRoster),

   'ok'.

-spec generate_reply () -> binary().
generate_reply () ->
   jiffy:encode([shr_okay:generate()]).

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   case authenticate_user(Input) of
      {ok, Player} ->
         shr_security:lock_queries(Input#input.player_id),
         QueryState = fetch_data(Player, Input),
         Update = update_data(QueryState, Input),
         commit_update(Update, Input),
         shr_security:unlock_queries(Input#input.player_id),
         generate_reply();

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
