-module(character_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   character_instance,
   {
      character :: character:struct(),
      location :: {non_neg_integer(), non_neg_integer()},
      current_health :: non_neg_integer(),
      active :: boolean()
   }
).

-opaque struct() :: #character_instance{}.

-export_type([struct/0]).

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
-spec find_random_location
   (
      non_neg_integer(),
      non_neg_integer(),
      list({non_neg_integer(), non_neg_integer()})
   )
   -> {non_neg_integer(), non_neg_integer()}.
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
-spec get_character (struct()) -> character:struct().
get_character (CharInst) -> CharInst#character_instance.character.

-spec get_location (struct()) -> {non_neg_integer(), non_neg_integer()}.
get_location (CharInst) -> CharInst#character_instance.location.

-spec get_current_health (struct()) -> non_neg_integer().
get_current_health (CharInst) -> CharInst#character_instance.current_health.

-spec get_is_active (struct()) -> boolean().
get_is_active (CharInst) ->
   (
      CharInst#character_instance.active
      and
      (CharInst#character_instance.current_health > 0)
   ).

-spec set_character (character:struct(), struct()) -> struct().
set_character (Char, CharInst) ->
   CharInst#character_instance
   {
      character = Char
   }.

-spec set_location
   (
      {non_neg_integer(), non_neg_integer()},
      struct()
   )
   -> struct().
set_location (Location, CharInst) ->
   CharInst#character_instance
   {
      location = Location
   }.

-spec set_current_health (non_neg_integer(), struct()) -> struct().
set_current_health (Health, CharInst) ->
   CharInst#character_instance
   {
      current_health = Health
   }.

-spec set_is_active (boolean(), struct()) -> struct().
set_is_active (Active, CharInst) ->
   CharInst#character_instance
   {
      active = Active
   }.

%%%% Utils
-spec new
   (
      character:struct(),
      {non_neg_integer(), non_neg_integer()}
   )
   -> struct().
new (Character, Location) ->
   CharacterStatistics = character:get_statistics(Character),
   #character_instance
   {
      character = Character,
      location = Location,
      current_health = statistics:get_health(CharacterStatistics),
      active = false
   }.

-spec random
   (
      character:struct(),
      non_neg_integer(),
      non_neg_integer(),
      list({non_neg_integer(), non_neg_integer()})
   )
   -> struct().
random (Character, BattlemapWidth, BattlemapHeight, ForbiddenLocations) ->
   new
   (
      Character,
      find_random_location(BattlemapWidth, BattlemapHeight, ForbiddenLocations)
   ).
