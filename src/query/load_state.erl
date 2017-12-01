-module(load_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").

-record
(
   input,
   {
      session_token,
      player_id,
      battlemap_id,
      instance_id
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
   database_shim:assert_session_is_valid(PlayerID, SessionToken),
   #input
   {
      player_id = PlayerID,
      battlemap_id = maps:get(<<"battlemap_id">>, JSONReqMap),
      instance_id = maps:get(<<"instance_id">>, JSONReqMap)
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
   %%%% Parse
   Input = parse_input(Req),
   %%%% Fetch
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
