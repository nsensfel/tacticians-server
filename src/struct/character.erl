-module(character).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque id() :: non_neg_integer().

-record
(
   character,
   {
      id :: id(),
      owner_id :: player:id(),
      name :: binary(),
      icon :: binary(),
      portrait :: binary(),
      attributes :: attributes:struct(),
      statistics :: statistics:struct(),
      weapon_ids :: {weapon:id(), weapon:id()}
   }
).

-opaque struct() :: #character{}.

-export_type([struct/0, id/0]).
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
-spec get_id (struct()) -> id().
get_id (Char) -> Char#character.id.

-spec get_owner_id (struct()) -> player:id().
get_owner_id (Char) -> Char#character.owner_id.

-spec get_name (struct()) -> binary().
get_name (Char) -> Char#character.name.

-spec get_icon (struct()) -> binary().
get_icon (Char) -> Char#character.icon.

-spec get_portrait (struct()) -> binary().
get_portrait (Char) -> Char#character.portrait.

-spec get_attributes (struct()) -> attributes:struct().
get_attributes (Char) -> Char#character.attributes.

-spec get_weapon_ids (struct()) -> {weapon:id(), weapon:id()}.
get_weapon_ids (Char) -> Char#character.weapon_ids.

-spec get_statistics (struct()) -> statistics:struct().
get_statistics (Char) -> Char#character.statistics.

-spec set_weapon_ids
   (
      {weapon:id(), weapon:id()},
      struct()
   )
   -> struct().
set_weapon_ids (WeaponIDs, Char) ->
   Char#character
   {
      weapon_ids = WeaponIDs
   }.

-spec set_statistics
   (
      statistics:struct(),
      struct()
   )
   -> struct().
set_statistics (Stats, Char) ->
   Char#character
   {
      statistics = Stats
   }.

-spec random
   (
      id(),
      player:id()
   )
   -> struct().
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
      statistics = Statistics
   }.
