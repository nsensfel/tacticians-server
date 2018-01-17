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
      min_dmg,
      max_dmg,
      hit_chance,
      double_hit_chance
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
   #character_instance
   {
      x = X,
      y = Y,
      health = character:get_max_health(Char),
      team = Owner,
      active_wp = 0
   }.

switch_weapon (CharInst, Char) ->
   {NewWpIndex, Weapon} = get_new_weapon(CharInst, Char),
   WeaponProf = character:get_proficiency(Char, weapon:get_type(Weapon)),
   {HitChance, DoubleHitChance} = calc_stats:weapon_hit_chances(WeaponProf),
   CharInst#character_instance
   {
      active_wp = NewWpIndex,
      min_dmg = calc_stats:weapon_min_damage(Weapon, WeaponProf),
      max_dmg = weapon:get_max_damage(Weapon),
      hit_chance = HitChance,
      double_hit_chance = DoubleHitChance
   }.

is_dead (CharInst) -> (CharInst#character_instance.health == 0).
