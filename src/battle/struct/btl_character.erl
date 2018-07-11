-module(btl_character).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: non_neg_integer().
-type rank() :: ('optional' | 'target' | 'commander').

-record
(
   character,
   {
      id :: id(),
      player_ix :: non_neg_integer(),
      name :: binary(),
      rank :: rank(),
      icon :: binary(),
      portrait :: binary(),
      attributes :: shr_attributes:type(),
      statistics :: shr_statistics:type(),
      weapon_ids :: {shr_weapon:id(), shr_weapon:id()},
      armor_id :: shr_armor:id(),
      location :: {non_neg_integer(), non_neg_integer()},
      current_health :: integer(), %% Negative integers let us reverse attacks.
      is_active :: boolean(),
      is_defeated :: boolean()
   }
).

-opaque type() :: #character{}.

-export_type([type/0, rank/0, id/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_player_index/1,
      get_name/1,
      get_rank/1,
      get_icon/1,
      get_portrait/1,
      get_attributes/1,
      get_statistics/1,
      get_weapon_ids/1,
      get_armor_id/1,
      get_location/1,
      get_current_health/1,
      get_is_alive/1,
      get_is_active/1,
      get_is_defeated/1,

      set_rank/2,
      set_weapon_ids/2,
      set_armor_id/2,
      set_statistics/2,
      set_location/2,
      set_current_health/2,
      set_is_active/2,
      set_is_defeated/2,

      get_rank_field/0,
      get_statistics_field/0,
      get_weapons_field/0,
      get_location_field/0,
      get_current_health_field/0,
      get_is_active_field/0,
      get_is_defeated_field/0
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
   X = shr_roll:between(0, (BattlemapWidth - 1)),
   Y = shr_roll:between(0, (BattlemapHeight - 1)),

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

-spec get_player_index (type()) -> non_neg_integer().
get_player_index (Char) -> Char#character.player_ix.

-spec get_name (type()) -> binary().
get_name (Char) -> Char#character.name.

-spec get_rank (type()) -> rank().
get_rank (Char) -> Char#character.rank.

-spec get_icon (type()) -> binary().
get_icon (Char) -> Char#character.icon.

-spec get_portrait (type()) -> binary().
get_portrait (Char) -> Char#character.portrait.

-spec get_attributes (type()) -> shr_attributes:type().
get_attributes (Char) -> Char#character.attributes.

-spec get_armor_id (type()) -> shr_armor:id().
get_armor_id (Char) -> Char#character.armor_id.

-spec get_weapon_ids (type()) -> {shr_weapon:id(), shr_weapon:id()}.
get_weapon_ids (Char) -> Char#character.weapon_ids.

-spec get_statistics (type()) -> shr_statistics:type().
get_statistics (Char) -> Char#character.statistics.

-spec get_location (type()) -> {non_neg_integer(), non_neg_integer()}.
get_location (Char) -> Char#character.location.

-spec get_current_health (type()) -> integer().
get_current_health (Char) -> Char#character.current_health.

-spec get_is_alive (type()) -> boolean().
get_is_alive (Char) ->
   (
      (not Char#character.is_defeated)
      and (Char#character.current_health > 0)
   ).

-spec get_is_active (type()) -> boolean().
get_is_active (Char) ->
   (
      (not Char#character.is_defeated)
      and Char#character.is_active
      and get_is_alive(Char)
   ).

-spec get_is_defeated (type()) -> boolean().
get_is_defeated (Char) -> Char#character.is_defeated.

-spec set_rank (rank(), type()) -> type().
set_rank (Rank, Char) ->
   Char#character
   {
      rank = Rank
   }.

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

-spec set_current_health (integer(), type()) -> type().
set_current_health (Health, Char) ->
   Char#character
   {
      current_health = Health
   }.

-spec set_is_active (boolean(), type()) -> type().
set_is_active (Active, Char) ->
   Char#character
   {
      is_active = Active
   }.

-spec set_is_defeated (boolean(), type()) -> type().
set_is_defeated (Defeated, Char) ->
   Char#character
   {
      is_defeated = Defeated
   }.

-spec set_armor_id (shr_armor:id(), type()) -> type().
set_armor_id (ArmorID, Char) ->
   Char#character
   {
      armor_id = ArmorID
   }.

-spec set_weapon_ids ({shr_weapon:id(), shr_weapon:id()}, type()) -> type().
set_weapon_ids (WeaponIDs, Char) ->
   Char#character
   {
      weapon_ids = WeaponIDs
   }.

-spec set_statistics
   (
      shr_statistics:type(),
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
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      list({non_neg_integer(), non_neg_integer()})
   )
   -> type().
random (ID, PlayerIX, BattlemapWidth, BattlemapHeight, ForbiddenLocations) ->
   Location =
      find_random_location(BattlemapWidth, BattlemapHeight, ForbiddenLocations),
   WeaponIDs = {shr_weapon:random_id(), shr_weapon:random_id()},
   ArmorID = shr_armor:random_id(),
   Attributes = shr_attributes:random(),
   Statistics = shr_statistics:new(Attributes, WeaponIDs, ArmorID),
   IDAsListString = integer_to_list(ID),
   IDAsBinaryString = list_to_binary(IDAsListString),

   #character
   {
      id = ID,
      player_ix = PlayerIX,
      name = list_to_binary("Char" ++ IDAsListString),
      rank =
         if
            ((ID rem 8) == 0) -> commander;
            ((ID rem 3) == 0) -> target;
            true -> optional
         end,
      icon = IDAsBinaryString,
      portrait = IDAsBinaryString,
      attributes = Attributes,
      weapon_ids = WeaponIDs,
      armor_id = ArmorID,
      statistics = Statistics,
      location = Location,
      current_health = shr_statistics:get_health(Statistics),
      is_active = false,
      is_defeated = false
   }.

-spec get_rank_field() -> non_neg_integer().
get_rank_field () -> #character.rank.
-spec get_statistics_field() -> non_neg_integer().
get_statistics_field () -> #character.statistics.
-spec get_weapons_field() -> non_neg_integer().
get_weapons_field () -> #character.weapon_ids.
-spec get_location_field() -> non_neg_integer().
get_location_field () -> #character.location.
-spec get_current_health_field() -> non_neg_integer().
get_current_health_field () -> #character.current_health.
-spec get_is_active_field() -> non_neg_integer().
get_is_active_field () -> #character.is_active.
-spec get_is_defeated_field() -> non_neg_integer().
get_is_defeated_field () -> #character.is_defeated.
