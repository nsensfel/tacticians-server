-module(btl_character_current_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   character_current_data,
   {
      attributes :: shr_attributes:type(),
      statistics :: shr_statistics:type(),
      omnimods :: shr_omnimods:type()
   }
).

-opaque type() :: #character_current_data{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_attributes/1,
      get_statistics/1,
      get_omnimods/1
   ]
).

-export
(
   [
      new/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec location_to_omnimods
   (
      {non_neg_integer(), non_neg_integer()},
      shr_map:type()
   )
   -> shr_omnimods:type().
location_to_omnimods (Location, Map) ->
   TileInstance = shr_map:get_tile_instance(Location, Map),
   TileClassID = shr_tile_instance:get_tile_id(TileInstance),
   Tile = shr_tile:from_id(TileClassID),

   shr_tile:get_omnimods(Tile).

-spec weapon_id_to_omnimods (shr_weapon:id()) -> shr_omnimods:type().
weapon_id_to_omnimods (WeaponID) ->
   Weapon = shr_weapon:from_id(WeaponID),

   shr_weapon:get_omnimods(Weapon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_omnimods (type()) -> shr_omnimods:type().
get_omnimods (Char) -> Char#character_current_data.omnimods.

-spec get_attributes (type()) -> shr_attributes:type().
get_attributes (Char) -> Char#character_current_data.attributes.

-spec get_statistics (type()) -> shr_statistics:type().
get_statistics (Char) -> Char#character_current_data.statistics.

%%%% Utils
-spec new (btl_character:type(), shr_map:type()) -> type().
new (Character, Map) ->
   PermanentOmnimods = btl_character:get_permanent_omnimods(Character),

   {WeaponID, _} = btl_character:get_weapon_ids(Character),
   WeaponOmnimods = weapon_id_to_omnimods(WeaponID),

   Location = btl_character:get_location(Character),
   TileOmnimods = location_to_omnimods(Location, Map),

   CurrentOmnimods =
      shr_omnimods:merge
      (
         shr_omnimods:merge(WeaponOmnimods, TileOmnimods),
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

   #character_current_data
   {
      attributes = CurrentAttributes,
      statistics = CurrentStatistics,
      omnimods = CurrentOmnimods
   }.

