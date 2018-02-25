-module(load_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id,
      session_token,
      battlemap_instance_id
   }
).

-record
(
   query_state,
   {
      battlemap_instance
   }
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_input (Req) ->
   JSONReqMap = jiffy:decode(Req, [return_maps]),
   PlayerID = maps:get(<<"pid">>, JSONReqMap),
   SessionToken =  maps:get(<<"stk">>, JSONReqMap),
   BattlemapInstanceID = maps:get(<<"bmi">>, JSONReqMap),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      battlemap_instance_id = BattlemapInstanceID
   }.

fetch_data (Input) ->
   PlayerID = Input#input.player_id,
   BattlemapInstanceID = Input#input.battlemap_instance_id,

   BattlemapInstance =
      timed_cache:fetch
      (
         battlemap_instance_db,
         PlayerID,
         BattlemapInstanceID
      ),

   #query_state
   {
      battlemap_instance = BattlemapInstance
   }.

generate_reply (QueryState) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   jiffy:encode
   (
      [
         set_map:generate(battlemap_instange:get_battlemap(BattlemapInstance))
         |
         array:to_list
         (
            array:map
            (
               fun (CharacterInstance) ->
                  add_char:generate(CharacterInstance)
               end,
               battlemap_instance:get_character_instances(BattlemapInstance)
            )
         )
      ]
   ).

handle (Req) ->
   Input = parse_input(Req),
   security:assert_identity(Input#input.player_id, Input#input.session_token),
   security:lock_queries(Input#input.player_id),
   QueryState = fetch_data(Input),
   security:unlock_queries(Input#input.player_id),
   generate_reply(QueryState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
