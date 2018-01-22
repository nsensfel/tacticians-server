-module(character_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   character_instance,
   {
      x,
      y,
      health,
      team,
      active_wp,
      stats
   }
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_location/1,
      get_current_health/1,
      get_owner/1,
      get_active_weapon/2,
      get_statistics/1,
      set_location/3,
      mod_health/3
   ]
).

%%%% Utils
-export
(
   [
      new_instance_of/3,
      switch_weapon/2,
      is_dead/1 % is_alive is reserved.
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_new_weapon(CharInst, Char) ->
   case CharInst#character_instance.active_wp of
      0 ->
         {_, Weapon} = character:get_weapons(Char),
         {1, Weapon};

      1 ->
         {Weapon, _} = character:get_weapons(Char),
         {0, Weapon}
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_location (CharInst) ->
   {CharInst#character_instance.x, CharInst#character_instance.y}.

get_current_health (CharInst) -> CharInst#character_instance.health.

get_owner (CharInst) -> CharInst#character_instance.team.

get_active_weapon (CharInst, Char) ->
   case CharInst#character_instance.active_wp of
      0 ->
         {_, Weapon} = character:get_weapons(Char),
         Weapon;

      1 ->
         {Weapon, _} = character:get_weapons(Char),
         Weapon
   end.

get_statistics (CharInst) -> CharInst#character_instance.stats.

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

%%%% Utils
new_instance_of (Char, Owner, {X, Y}) ->
   {Weapon, _} = character:get_weapons(Char),
   Stats = statistics:calc_for(character:get_attributes(Char), Weapon),
   #character_instance
   {
      x = X,
      y = Y,
      health = statistics:get_health(Stats),
      team = Owner,
      stats = Stats,
      active_wp = 0
   }.

switch_weapon (CharInst, Char) ->
   {NewWpIndex, Weapon} = get_new_weapon(CharInst, Char),
   CharInst#character_instance
   {
      active_wp = NewWpIndex,
      stats = statistics:calc_for(character:get_attributes(Char), Weapon)
   }.

is_dead (CharInst) -> (CharInst#character_instance.health == 0).
