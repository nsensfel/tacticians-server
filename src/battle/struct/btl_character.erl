-module(btl_character).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type rank() :: ('optional' | 'target' | 'commander').

-record
(
   btl_char,
   {
      player_ix :: non_neg_integer(),
      name :: binary(),
      rank :: rank(),
      portrait_id :: shr_portrait:id(),
      weapon_ids :: {shr_weapon:id(), shr_weapon:id()},
      armor_id :: shr_armor:id(),
      location :: {non_neg_integer(), non_neg_integer()},
      current_health :: integer(), %% Negative integers let us reverse attacks.
      is_active :: boolean(),
      is_defeated :: boolean(),
      permanent_omnimods :: shr_omnimods:type()
   }
).

-opaque type() :: #btl_char{}.

-export_type([type/0, rank/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_player_index/1,
      get_name/1,
      get_rank/1,
      get_portrait_id/1,
      get_weapon_ids/1,
      get_armor_id/1,
      get_location/1,
      get_current_health/1,
      get_is_alive/1,
      get_is_active/1,
      get_is_defeated/1,
      get_permanent_omnimods/1,

      set_rank/2,
      set_weapon_ids/2,
      set_armor_id/2,
      set_location/2,
      set_current_health/2,
      set_is_active/2,
      set_is_defeated/2,

      get_rank_field/0,
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
      new/9
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_player_index (type()) -> non_neg_integer().
get_player_index (Char) -> Char#btl_char.player_ix.

-spec get_name (type()) -> binary().
get_name (Char) -> Char#btl_char.name.

-spec get_rank (type()) -> rank().
get_rank (Char) -> Char#btl_char.rank.

-spec get_portrait_id (type()) -> shr_portrait:id().
get_portrait_id (Char) -> Char#btl_char.portrait_id.

-spec get_armor_id (type()) -> shr_armor:id().
get_armor_id (Char) -> Char#btl_char.armor_id.

-spec get_weapon_ids (type()) -> {shr_weapon:id(), shr_weapon:id()}.
get_weapon_ids (Char) -> Char#btl_char.weapon_ids.

-spec get_location (type()) -> {non_neg_integer(), non_neg_integer()}.
get_location (Char) -> Char#btl_char.location.

-spec get_current_health (type()) -> integer().
get_current_health (Char) -> Char#btl_char.current_health.

-spec get_permanent_omnimods (type()) -> shr_omnimods:type().
get_permanent_omnimods (Char) -> Char#btl_char.permanent_omnimods.

-spec get_is_alive (type()) -> boolean().
get_is_alive (Char) ->
   (
      (not Char#btl_char.is_defeated)
      and (Char#btl_char.current_health > 0)
   ).

-spec get_is_active (type()) -> boolean().
get_is_active (Char) ->
   (
      (not Char#btl_char.is_defeated)
      and Char#btl_char.is_active
      and get_is_alive(Char)
   ).

-spec get_is_defeated (type()) -> boolean().
get_is_defeated (Char) -> Char#btl_char.is_defeated.

-spec set_rank (rank(), type()) -> type().
set_rank (Rank, Char) ->
   Char#btl_char
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
   Char#btl_char
   {
      location = Location
   }.

-spec set_current_health (integer(), type()) -> type().
set_current_health (Health, Char) ->
   Char#btl_char
   {
      current_health = Health
   }.

-spec set_is_active (boolean(), type()) -> type().
set_is_active (Active, Char) ->
   Char#btl_char
   {
      is_active = Active
   }.

-spec set_is_defeated (boolean(), type()) -> type().
set_is_defeated (Defeated, Char) ->
   Char#btl_char
   {
      is_defeated = Defeated
   }.

-spec set_armor_id (shr_armor:id(), type()) -> type().
set_armor_id (ArmorID, Char) ->
   Char#btl_char
   {
      armor_id = ArmorID
   }.

-spec set_weapon_ids ({shr_weapon:id(), shr_weapon:id()}, type()) -> type().
set_weapon_ids (WeaponIDs, Char) ->
   Char#btl_char
   {
      weapon_ids = WeaponIDs
   }.

%%%% Utils
-spec new
   (
      non_neg_integer(),
      binary(),
      rank(),
      shr_omnimods:type(),
      shr_portrait:id(),
      {shr_weapon:id(), shr_weapon:id()},
      shr_armor:id(),
      btl_location:type(),
      shr_omnimods:type()
   )
   -> type().
new
(
   PlayerIX,
   Name,
   Rank,
   GlyphsOmnimods,
   PortraitID,
   WeaponIDs,
   ArmorID,
   Location,
   LocationOmnimods
) ->
   {MainWeaponID, _} = WeaponIDs,

   Armor = shr_armor:from_id(ArmorID),
   MainWeapon = shr_weapon:from_id(MainWeaponID),

   PermanentOmnimods =
      shr_omnimods:merge(shr_armor:get_omnimods(Armor), GlyphsOmnimods),

   CurrentOmnimods =
      shr_omnimods:merge
      (
         shr_omnimods:merge
         (
            shr_weapon:get_omnimods(MainWeapon),
            LocationOmnimods
         ),
         PermanentOmnimods
      ),

   CurrentAttributes =
      shr_omnimods:apply_to_attributes
      (
         CurrentOmnimods,
         shr_attributes:default()
      ),

   CurrentStatistics =
      shr_omnimods:apply_to_statistics
      (
         CurrentOmnimods,
         shr_statistics:new_raw(CurrentAttributes)
      ),

   #btl_char
   {
      player_ix = PlayerIX,
      name = Name,
      rank = Rank,
      portrait_id = PortraitID,
      weapon_ids = WeaponIDs,
      armor_id = ArmorID,
      location = Location,
      current_health = shr_statistics:get_health(CurrentStatistics),
      is_active = false,
      is_defeated = false,
      permanent_omnimods = PermanentOmnimods
   }.

-spec get_rank_field() -> non_neg_integer().
get_rank_field () -> #btl_char.rank.
-spec get_weapons_field() -> non_neg_integer().
get_weapons_field () -> #btl_char.weapon_ids.
-spec get_location_field() -> non_neg_integer().
get_location_field () -> #btl_char.location.
-spec get_current_health_field() -> non_neg_integer().
get_current_health_field () -> #btl_char.current_health.
-spec get_is_active_field() -> non_neg_integer().
get_is_active_field () -> #btl_char.is_active.
-spec get_is_defeated_field() -> non_neg_integer().
get_is_defeated_field () -> #btl_char.is_defeated.
