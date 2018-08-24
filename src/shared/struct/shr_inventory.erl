-module(shr_inventory).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   inventory,
   {
      owner_id :: shr_player:id(),
      portrait_ids :: sets:set(binary()),
      glyph_ids :: sets:set(binary()),
      glyph_board_ids :: sets:set(binary()),
      weapon_ids :: sets:set(binary()),
      armor_ids :: sets:set(binary())
   }
).

-opaque type() :: #inventory{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_owner_id/1,

      get_portrait_ids/1,
      get_glyph_ids/1,
      get_glyph_board_ids/1,
      get_weapon_ids/1,
      get_armor_ids/1,

      set_portrait_ids/2,
      set_glyph_ids/2,
      set_glyph_board_ids/2,
      set_weapon_ids/2,
      set_armor_ids/2
   ]
).

-export
(
   [
      get_portrait_ids_field/0,
      get_glyph_ids_field/0,
      get_glyph_board_ids_field/0,
      get_weapon_ids_field/0,
      get_armor_ids_field/0
   ]
).

-export
(
   [
      new/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_owner_id (type()) -> shr_player:id().
get_owner_id (Inv) -> Inv#inventory.owner_id.

-spec get_portrait_ids (type()) -> sets:set(binary()).
get_portrait_ids (Inv) -> Inv#inventory.portrait_ids.

-spec get_glyph_ids (type()) -> sets:set(binary()).
get_glyph_ids (Inv) -> Inv#inventory.glyph_ids.

-spec get_glyph_board_ids (type()) -> sets:set(binary()).
get_glyph_board_ids (Inv) -> Inv#inventory.glyph_board_ids.

-spec get_weapon_ids (type()) -> sets:set(binary()).
get_weapon_ids (Inv) -> Inv#inventory.weapon_ids.

-spec get_armor_ids (type()) -> sets:set(binary()).
get_armor_ids (Inv) -> Inv#inventory.armor_ids.

-spec set_portrait_ids (sets:set(binary()), type()) -> type().
set_portrait_ids (Value, Inv) ->
   Inv#inventory
   {
      portrait_ids = Value
   }.

-spec set_glyph_ids (sets:set(binary()), type()) -> type().
set_glyph_ids (Value, Inv) ->
   Inv#inventory
   {
      glyph_ids = Value
   }.

-spec set_glyph_board_ids (sets:set(binary()), type()) -> type().
set_glyph_board_ids (Value, Inv) ->
   Inv#inventory
   {
      glyph_board_ids = Value
   }.

-spec set_weapon_ids (sets:set(binary()), type()) -> type().
set_weapon_ids (Value, Inv) ->
   Inv#inventory
   {
      weapon_ids = Value
   }.

-spec set_armor_ids (sets:set(binary()), type()) -> type().
set_armor_ids (Value, Inv) ->
   Inv#inventory
   {
      armor_ids = Value
   }.

-spec get_portrait_ids_field () -> non_neg_integer().
get_portrait_ids_field () -> #inventory.portrait_ids.

-spec get_glyph_ids_field () -> non_neg_integer().
get_glyph_ids_field () -> #inventory.glyph_ids.

-spec get_glyph_board_ids_field () -> non_neg_integer().
get_glyph_board_ids_field () -> #inventory.glyph_board_ids.

-spec get_weapon_ids_field () -> non_neg_integer().
get_weapon_ids_field () -> #inventory.weapon_ids.

-spec get_armor_ids_field () -> non_neg_integer().
get_armor_ids_field () -> #inventory.armor_ids.

-spec new (shr_player:id()) -> type().
new (OwnerID) ->
   EmptySet = sets:new(),

   #inventory
   {
      owner_id = OwnerID,
      portrait_ids = EmptySet,
      glyph_ids = EmptySet,
      glyph_board_ids = EmptySet,
      weapon_ids = EmptySet,
      armor_ids = EmptySet
   }.
