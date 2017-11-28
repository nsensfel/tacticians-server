-module(character_instance).
-record
(
   character_instance,
   {
      x,
      y,
      health,
      team
   }
).
-export
(
   [
      get_location/1,
      get_current_health/1,
      get_owner/1
   ]
).
-export
(
   [
      new_instance_of/3
   ]
).
-export
(
   [
      set_location/3,
      mod_health/3,
      is_dead/1 % is_alive is reserved.
   ]
).

get_location (CharInst) ->
   {CharInst#character_instance.x, CharInst#character_instance.y}.

get_current_health (CharInst) -> CharInst#character_instance.health.

get_owner (CharInst) -> CharInst#character_instance.team.

new_instance_of (Char, Owner, {X, Y}) ->
   #character_instance
   {
      x = X,
      y = Y,
      health = character:get_max_health(Char),
      team = Owner
   }.

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

