-module(battlemap_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   battlemap_instance,
   {
      id,
      chars,
      curr_player,
      players,
      rem_chars,
      last_turn
   }
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      list_characters/1,
      get_char_instances/1,
      get_char_instance/2,
      set_char_instance/3
   ]
).

%%%% Utils
-export
(
   [
      can_play_char_instance/3,
      post_play_char_instance/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_id (BattlemapInstance) -> BattlemapInstance#battlemap_instance.id.

list_characters (BattlemapInstance) ->
   dict:to_list(BattlemapInstance#battlemap_instance.chars).

get_char_instances (BattlemapInstance) ->
   lists:map
   (
      fun ({_K, V}) -> V end,
      dict:to_list(BattlemapInstance#battlemap_instance.chars)
   ).

can_play_char_instance
(
   BattlemapInstance,
   PlayerID,
   CharInstID
) ->
   (
      (
         array:get
         (
            BattlemapInstance#battlemap_instance.curr_player,
            BattlemapInstance#battlemap_instance.players
         )
         =:=
         PlayerID
      )
      and
      lists:member(CharInstID, BattlemapInstance#battlemap_instance.rem_chars)
   ).

post_play_char_instance (BattlemapInstance, CharInstID) ->
   case BattlemapInstance#battlemap_instance.rem_chars of
      [CharInstID|[]] ->
         NextPlayer =
            (
               (BattlemapInstance#battlemap_instance.curr_player + 1)
               rem
               array:size(BattlemapInstance#battlemap_instance.players)
            ),
         BattlemapInstance#battlemap_instance
         {
            curr_player = NextPlayer,
            rem_chars =
               lists:filtermap
               (
                  fun ({K, V}) ->
                     case character_instance:get_owner(V) of
                        NextPlayer -> {true, K};
                        _ -> false
                     end
                  end,
                  dict:to_list(BattlemapInstance#battlemap_instance.chars)
               )
         };

      _ ->
         BattlemapInstance#battlemap_instance
         {
            rem_chars =
               lists:delete
               (
                  CharInstID,
                  BattlemapInstance#battlemap_instance.rem_chars
               )
         }
   end.

get_char_instance (BattlemapInstance, CharInstID) ->
   dict:fetch(CharInstID, BattlemapInstance#battlemap_instance.chars).

set_char_instance (BattlemapInstance, CharInstID, CharInst) ->
   BattlemapInstance#battlemap_instance
   {
      chars =
         dict:store
         (
            CharInstID,
            CharInst,
            BattlemapInstance#battlemap_instance.chars
         )
   }.
