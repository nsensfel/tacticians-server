-module(battlemap_instance).
-export
(
   [
      get_char_instances/1,
      get_char_instance/2,
      set_char_instance/3,
      can_play_char_instance/3,
      post_play_char_instance/2
   ]
).

-include("timed_cache_data.hrl").

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
