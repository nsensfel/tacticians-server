-module(character_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   character_instance,
   {
      character :: character:type(),
      location :: {non_neg_integer(), non_neg_integer()},
      current_health :: non_neg_integer(),
      active :: boolean()
   }
).

-opaque type() :: #character_instance{}.

-export_type([type/0]).

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
      get_is_alive/1,
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
-spec get_character (type()) -> character:type().
get_character (CharInst) -> CharInst#character_instance.character.

-spec get_location (type()) -> {non_neg_integer(), non_neg_integer()}.
get_location (CharInst) ->
   true = get_is_alive(CharInst),
   CharInst#character_instance.location.

-spec get_current_health (type()) -> non_neg_integer().
get_current_health (CharInst) -> CharInst#character_instance.current_health.

-spec get_is_alive (type()) -> boolean().
get_is_alive (CharInst) ->
   (CharInst#character_instance.current_health > 0).

-spec get_is_active (type()) -> boolean().
get_is_active (CharInst) ->
   (
      CharInst#character_instance.active
      and
      get_is_alive(CharInst)
   ).

-spec set_character (character:type(), type()) -> type().
set_character (Char, CharInst) ->
   CharInst#character_instance
   {
      character = Char
   }.

-spec set_location
   (
      {non_neg_integer(), non_neg_integer()},
      type()
   )
   -> type().
set_location (Location, CharInst) ->
   CharInst#character_instance
   {
      location = Location
   }.

-spec set_current_health (non_neg_integer(), type()) -> type().
set_current_health (Health, CharInst) ->
   CharInst#character_instance
   {
      current_health = max(0, Health)
   }.

-spec set_is_active (boolean(), type()) -> type().
set_is_active (Active, CharInst) ->
   CharInst#character_instance
   {
      active = Active
   }.

%%%% Utils
-spec new
   (
      character:type(),
      {non_neg_integer(), non_neg_integer()}
   )
   -> type().
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
      character:type(),
      non_neg_integer(),
      non_neg_integer(),
      list({non_neg_integer(), non_neg_integer()})
   )
   -> type().
random (Character, BattlemapWidth, BattlemapHeight, ForbiddenLocations) ->
   new
   (
      Character,
      find_random_location(BattlemapWidth, BattlemapHeight, ForbiddenLocations)
   ).
