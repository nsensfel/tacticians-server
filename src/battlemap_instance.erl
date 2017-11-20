-module(battlemap_instance).
-export
(
   [
      get_char_instances/1,
      get_char_instance/2,
      set_char_instance/3
   ]
).

-include("timed_cache_data.hrl").

get_char_instances (BattlemapInstance) ->
   lists:map
   (
      fun ({_K, V}) -> V end,
      dict:to_list(BattlemapInstance#battlemap_instance.chars)
   ).

get_char_instance (BattlemapInstance, CharInstID) ->
   {ok, dict:fetch(CharInstID, BattlemapInstance#battlemap_instance.chars)}.

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
