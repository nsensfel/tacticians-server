-module(character_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   character_instance,
   {
      character,
      location,
      current_health,
      active
   }
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      new/2,
      random/4
   ]
).

%%%% Accessors
-export
(
   [
      get_character/1,
      get_location/1,
      get_current_health/1,
      get_is_active/1,

      set_character/2,
      set_location/2,
      set_current_health/2,
      set_is_active/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_random_location (BattlemapWidth, BattlemapHeight, ForbiddenLocations) ->
   X = roll:between(0, (BattlemapWidth - 1)),
   Y = roll:between(0, (BattlemapHeight - 1)),

   IsForbidden = lists:member({X, Y}, ForbiddenLocations),

   case IsForbidden of
      true ->
         find_random_location
         (
            BattlemapWidth,
            BattlemapHeight,
            ForbiddenLocations
         );

      _ -> {X, Y}
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_character (CharInst) -> CharInst#character_instance.character.
get_location (CharInst) -> CharInst#character_instance.location.
get_current_health (CharInst) -> CharInst#character_instance.current_health.
get_is_active (CharInst) ->
   (
      CharInst#character_instance.active
      and
      (CharInst#character_instance.current_health > 0)
   ).

set_character (Char, CharInst) ->
   CharInst#character_instance
   {
      character = Char
   }.

set_location (Location, CharInst) ->
   CharInst#character_instance
   {
      location = Location
   }.

set_current_health (Health, CharInst) ->
   CharInst#character_instance
   {
      current_health = Health
   }.

set_is_active (Active, CharInst) ->
   CharInst#character_instance
   {
      active = Active
   }.

%%%% Utils
new (Character, Location) ->
   CharacterStatistics = character:get_statistics(Character),
   #character_instance
   {
      character = Character,
      location = Location,
      current_health = statistics:get_health(CharacterStatistics),
      active = false
   }.

random (Character, BattlemapWidth, BattlemapHeight, ForbiddenLocations) ->
   new
   (
      Character,
      find_random_location(BattlemapWidth, BattlemapHeight, ForbiddenLocations)
   ).
