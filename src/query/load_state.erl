-module(load_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: player:id(),
      session_token :: binary(),
      battlemap_instance_id :: binary()
   }
).

-record
(
   query_state,
   {
      battlemap_instance :: battlemap_instance:struct()
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
   BattlemapInstanceID = maps:get(<<"bmi">>, JSONReqMap),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      battlemap_instance_id = BattlemapInstanceID
   }.

-spec fetch_data (input()) -> query_state().
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

-spec generate_reply(query_state(), input()) -> binary().
generate_reply (QueryState, Input) ->
   PlayerID = Input#input.player_id,
   BattlemapInstance = QueryState#query_state.battlemap_instance,

   jiffy:encode
   (
      [
         set_map:generate(battlemap_instance:get_battlemap(BattlemapInstance))
         |
         array:sparse_to_list
         (
            array:map
            (
               fun (IX, CharacterInstance) ->
                  add_char:generate(IX, CharacterInstance, PlayerID)
               end,
               battlemap_instance:get_character_instances(BattlemapInstance)
            )
         )
      ]
   ).

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   security:assert_identity(Input#input.player_id, Input#input.session_token),
   security:lock_queries(Input#input.player_id),
   QueryState = fetch_data(Input),
   security:unlock_queries(Input#input.player_id),
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
