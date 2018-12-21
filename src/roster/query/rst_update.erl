-module(rst_update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-type decoded_character() :: {non_neg_integer(), rst_character:type()}.

-record
(
   input,
   {
      player_id :: binary(),
      session_token :: binary(),
      characters :: list(decoded_character())
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
-spec decode_character_list (list(map())) -> list(decoded_character()).
decode_character_list (EncodedCharactersList) ->
   lists:map
   (
      fun (Map) ->
         {maps:get(<<"ix">>, Map), rst_character:decode(Map)}
      end,
      EncodedCharactersList
   ).

-spec parse_input (binary()) -> input().
parse_input (Req) ->
   JSONReqMap = jiffy:decode(Req, [return_maps]),
   PlayerID = maps:get(<<"pid">>, JSONReqMap),
   SessionToken =  maps:get(<<"stk">>, JSONReqMap),
   EncodedCharacterList = maps:get(<<"rst">>, JSONReqMap),
   CharacterList = decode_character_list(EncodedCharacterList),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      characters = CharacterList
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
   _InventoryID = shr_player:get_inventory_id(Player),

   Roster =
      shr_timed_cache:fetch
      (
         roster_db,
         ataxia_security:user_from_id(PlayerID),
         RosterID
      ),

%   Inventory = shr_timed_cache:fetch(inventory_db, PlayerID, InventoryID),
   Inventory = shr_inventory:new(PlayerID),

   #query_state
   {
      player = Player,
      roster = Roster,
      inventory = Inventory
   }.

-spec update_data (query_state(), input()) -> query_state().
update_data (QueryState, Input) ->
   Inventory = QueryState#query_state.inventory,
   Characters = Input#input.characters,

   lists:map
   (
      fun ({_IX, Character}) ->
         rst_character:validate(Inventory, Character)
      end,
      Characters
   ),

   %% TODO [FUNCTION: chr][REQUIRED]: unimplemented.
   QueryState.

-spec commit_update (query_state(), input()) -> 'ok'.
commit_update (QueryState, Input) ->
   PlayerID = Input#input.player_id,
   Characters = Input#input.characters,
   Player = QueryState#query_state.player,
   Roster = QueryState#query_state.roster,

   RosterID = shr_player:get_roster_id(Player),

   {UpdatedRoster, QueryList} =
      lists:foldl
      (
         fun ({IX, Character}, {CurrentRoster, UpdateList}) ->
             {
               rst_roster:set_character(IX, Character, CurrentRoster),
               [
                  ataxic_sugar:update_orddict_element
                  (
                     IX,
                     ataxic:constant(Character)
                  )
                  | UpdateList
               ]
            }
         end,
         {Roster, []},
         Characters
      ),

   ok =
      ataxia_client:update
      (
         roster_db,
         ataxia_security:user_from_id(PlayerID),
         ataxic:update_value
         (
            ataxic:update_field
            (
               rst_roster:get_characters_field(),
               ataxic:sequence(QueryList)
            )
         ),
         RosterID
      ),

   shr_timed_cache:update
   (
      roster_db,
      ataxia_security:user_from_id(PlayerID),
      RosterID,
      UpdatedRoster
   ),

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
