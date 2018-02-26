-module(character).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   character,
   {
      id,
      owner_id,
      name,
      icon,
      portrait,
      attributes,
      statistics,
      glyphs,
      weapon_ids
   }
).

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
      get_glyphs/1,

      set_weapon_ids/2,
      set_statistics/2
   ]
).

-export
(
   [
      random/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_id (Char) -> Char#character.id.
get_owner_id (Char) -> Char#character.owner_id.
get_name (Char) -> Char#character.name.
get_icon (Char) -> Char#character.icon.
get_portrait (Char) -> Char#character.portrait.
get_attributes (Char) -> Char#character.attributes.
get_weapon_ids (Char) -> Char#character.weapon_ids.
get_glyphs (Char) -> Char#character.glyphs.

get_statistics (Char) -> Char#character.statistics.

set_weapon_ids (WeaponIDs, Char) ->
   Char#character
   {
      weapon_ids = WeaponIDs
   }.

set_statistics (Stats, Char) ->
   Char#character
   {
      statistics = Stats
   }.

random (ID, OwnerID) ->
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
      glyphs = [],
      statistics = Statistics
   }.
