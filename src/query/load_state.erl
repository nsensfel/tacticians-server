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
   PlayerID = maps:get(<<"player_id">>, JSONReqMap),
   SessionToken =  maps:get(<<"session_token">>, JSONReqMap),
   BattlemapInstanceID = maps:get(<<"battlemap_id">>, JSONReqMap),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      battlemap_instance_id = BattlemapInstanceID
   }.

generate_reply (Battlemap, BattlemapInstance, Characters, PlayerID) ->
   jiffy:encode
   (
      [
         set_map:generate(Battlemap)
         |
         lists:map
         (
            fun ({Char, CharInstance}) ->
               add_char:generate
               (
                  Char,
                  CharInstance,
                  (
                     battlemap_instance:can_play_char_instance
                     (
                        BattlemapInstance,
                        PlayerID,
                        character:get_id(Char)
                     )
                     and
                     (not character_instance:is_dead(CharInstance))
                  )
               )
            end,
            Characters
         )
      ]
   ).

handle (Req) ->
   Input = parse_input(Req),
   security:assert_identity(Input#input.player_id, Input#input.session_token),
   security:lock_queries(Input#input.player_id),
   Battlemap =
      timed_cache:fetch
      (
         battlemap_db,
         Input#input.player_id,
         Input#input.battlemap_id
      ),
   BattlemapInstance =
      timed_cache:fetch
      (
         battlemap_instance_db,
         Input#input.player_id,
         <<"0">>
      ),
   Characters =
      lists:map
      (
         fun ({CharID, CharInst}) ->
            {
               timed_cache:fetch(character_db, Input#input.player_id, CharID),
               CharInst
            }
         end,
         battlemap_instance:list_characters(BattlemapInstance)
      ),
   %%%% Calc
   %%%% Commit
   %%%% Reply
   generate_reply
   (
      Battlemap,
      BattlemapInstance,
      Characters,
      Input#input.player_id
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
