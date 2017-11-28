-module(character_instance).
-export
(
   [
      set_location/3,
      mod_health/3,
      is_dead/1, % is_alive is reserved.
      get_location/1,
      get_owner/1
   ]
).

-include("timed_cache_data.hrl").

set_location (CharInst, X, Y) ->
   CharInst#character_instance
   {
      x = X,
      y = Y
   }.

mod_health (CharInst, MaxHealth, HealthMod) ->
   NewHealth = (CharInst#character_instance.health + HealthMod),
   if
      (NewHealth < 0) ->
         CharInst#character_instance{ health = 0 };

      (NewHealth > MaxHealth) ->
         CharInst#character_instance{ health = MaxHealth };

      true ->
         CharInst#character_instance{ health = NewHealth }
   end.

is_dead (CharInst) -> (CharInst#character_instance.health == 0).

get_location (CharInst) ->
   {CharInst#character_instance.x, CharInst#character_instance.y}.

get_owner (CharInst) -> CharInst#character_instance.team.
