-module(character).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: non_neg_integer().

-record
(
   character,
   {
      id :: id(),
      owner_id :: player:id(),
      name :: binary(),
      icon :: binary(),
      portrait :: binary(),
      attributes :: attributes:type(),
      statistics :: statistics:type(),
      weapon_ids :: {weapon:id(), weapon:id()},
      location :: {non_neg_integer(), non_neg_integer()},
      current_health :: non_neg_integer(),
      active :: boolean()
   }
).

-opaque type() :: #character{}.

-export_type([type/0, id/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_owner_id/1,
      get_name/1,
      get_icon/1,
      get_portrait/1,
      get_attributes/1,
      get_statistics/1,
      get_weapon_ids/1,
      get_location/1,
      get_current_health/1,
      get_is_alive/1,
      get_is_active/1,

      set_weapon_ids/2,
      set_statistics/2,
      set_location/2,
      set_current_health/2,
      set_is_active/2
   ]
).

-export
(
   [
      random/5
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
-spec get_id (type()) -> id().
get_id (Char) -> Char#character.id.

-spec get_owner_id (type()) -> player:id().
get_owner_id (Char) -> Char#character.owner_id.

-spec get_name (type()) -> binary().
get_name (Char) -> Char#character.name.

-spec get_icon (type()) -> binary().
get_icon (Char) -> Char#character.icon.

-spec get_portrait (type()) -> binary().
get_portrait (Char) -> Char#character.portrait.

-spec get_attributes (type()) -> attributes:type().
get_attributes (Char) -> Char#character.attributes.

-spec get_weapon_ids (type()) -> {weapon:id(), weapon:id()}.
get_weapon_ids (Char) -> Char#character.weapon_ids.

-spec get_statistics (type()) -> statistics:type().
get_statistics (Char) -> Char#character.statistics.

-spec get_location (type()) -> {non_neg_integer(), non_neg_integer()}.
get_location (Char) ->
   true = get_is_alive(Char),
   Char#character.location.

-spec get_current_health (type()) -> non_neg_integer().
get_current_health (Char) -> Char#character.current_health.

-spec get_is_alive (type()) -> boolean().
get_is_alive (Char) ->
   (Char#character.current_health > 0).

-spec get_is_active (type()) -> boolean().
get_is_active (Char) ->
   (
      Char#character.active
      and
      get_is_alive(Char)
   ).

-spec set_location
   (
      {non_neg_integer(), non_neg_integer()},
      type()
   )
   -> type().
set_location (Location, Char) ->
   Char#character
   {
      location = Location
   }.

-spec set_current_health (non_neg_integer(), type()) -> type().
set_current_health (Health, Char) ->
   Char#character
   {
      current_health = max(0, Health)
   }.

-spec set_is_active (boolean(), type()) -> type().
set_is_active (Active, Char) ->
   Char#character
   {
      active = Active
   }.

-spec set_weapon_ids
   (
      {weapon:id(), weapon:id()},
      type()
   )
   -> type().
set_weapon_ids (WeaponIDs, Char) ->
   Char#character
   {
      weapon_ids = WeaponIDs
   }.

-spec set_statistics
   (
      statistics:type(),
      type()
   )
   -> type().
set_statistics (Stats, Char) ->
   Char#character
   {
      statistics = Stats
   }.

%%%% Utils
-spec random
   (
      non_neg_integer(),
      player:id(),
      non_neg_integer(),
      non_neg_integer(),
      list({non_neg_integer(), non_neg_integer()})
   )
   -> type().
random (ID, OwnerID, BattlemapWidth, BattlemapHeight, ForbiddenLocations) ->
   Location =
      find_random_location(BattlemapWidth, BattlemapHeight, ForbiddenLocations),
   WeaponIDs = {weapon:random_id(), weapon:random_id()},
   Attributes = attributes:random(),
   Statistics = statistics:new(Attributes, WeaponIDs),
   IDAsListString = integer_to_list(ID),
   IDAsBinaryString = list_to_binary(IDAsListString),

   #character
   {
      id = ID,
      owner_id = OwnerID,
      name = list_to_binary("Char" ++ IDAsListString),
      icon = IDAsBinaryString,
      portrait = IDAsBinaryString,
      attributes = Attributes,
      weapon_ids = WeaponIDs,
      statistics = Statistics,
      location = Location,
      current_health = statistics:get_health(Statistics),
      active = false
   }.
