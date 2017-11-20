-module(character_instance).
-export
(
   [
      set_location/3,
      get_location/1
   ]
).

-include("timed_cache_data.hrl").

set_location (CharInst, X, Y) ->
   CharInst#character_instance
   {
      x = X,
      y = Y
   }.

get_location (CharInst) ->
   {CharInst#character_instance.x, CharInst#character_instance.y}.
