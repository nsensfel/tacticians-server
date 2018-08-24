-module(chr_character).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   character,
   {
      name :: binary(),
      portrait :: binary(),
      weapon_ids :: {shr_weapon:id(), shr_weapon:id()},
      armor_id :: shr_armor:id(),
      glyph_ids :: array:array(shr_glyph:id()),
      glyph_board_id :: shr_glyph_board:id()
   }
).

-opaque type() :: #character{}.

-export_type([type/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_name/1,
      get_portrait/1,
      get_weapon_ids/1,
      get_armor_id/1,
      get_glyph_ids/1,
      get_glyph_board_id/1,

      set_name/2,
      set_portrait/2,
      set_weapon_ids/2,
      set_armor_id/2,
      set_glyph_ids/2,
      set_glyph_board_id/2,

      get_name_field/0,
      get_portrait_field/0,
      get_weapon_ids_field/0,
      get_armor_id_field/0,
      get_glyph_ids_field/0,
      get_glyph_board_id_field/0
   ]
).

-export
(
   [
      random/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_name (type()) -> binary().
get_name (Char) -> Char#character.name.

-spec get_portrait (type()) -> binary().
get_portrait (Char) -> Char#character.portrait.

-spec get_weapon_ids (type()) -> {shr_weapon:id(), shr_weapon:id()}.
get_weapon_ids (Char) -> Char#character.weapon_ids.

-spec get_armor_id (type()) -> shr_armor:id().
get_armor_id (Char) -> Char#character.armor_id.

-spec get_glyph_ids (type()) -> array:array(shr_glyph:id()).
get_glyph_ids (Char) -> Char#character.glyph_ids.

-spec get_glyph_board_id (type()) -> shr_glyph_board:id().
get_glyph_board_id (Char) -> Char#character.glyph_board_id.


-spec set_name (binary(), type()) -> type().
set_name (Name, Char) ->
   Char#character
   {
      name = Name
   }.

-spec set_portrait (binary(), type()) -> type().
set_portrait (PortraitID, Char) ->
   Char#character
   {
      portrait = PortraitID
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

-spec set_glyph_ids (array:array(shr_glyph:id()), type()) -> type().
set_glyph_ids (GlyphIDs, Char) ->
   Char#character
   {
      glyph_ids = GlyphIDs
   }.

-spec set_glyph_board_id (shr_glyph_board:id(), type()) -> type().
set_glyph_board_id (GlyphBoardID, Char) ->
   Char#character
   {
      glyph_board_id = GlyphBoardID
   }.

-spec random () -> type().
random () ->
   #character
   {
      name = <<"">>,
      portrait = <<"0">>,
      weapon_ids = {0, 0},
      armor_id = 0,
      glyph_ids = array:new(),
      glyph_board_id = <<"0">>
   }.

-spec get_name_field () -> non_neg_integer().
get_name_field () -> #character.name.
-spec get_portrait_field () -> non_neg_integer().
get_portrait_field () -> #character.portrait.
-spec get_armor_id_field () -> non_neg_integer().
get_armor_id_field () -> #character.armor_id.
-spec get_weapon_ids_field () -> non_neg_integer().
get_weapon_ids_field () -> #character.weapon_ids.
-spec get_glyph_ids_field () -> non_neg_integer().
get_glyph_ids_field () -> #character.glyph_ids.
-spec get_glyph_board_id_field () -> non_neg_integer().
get_glyph_board_id_field () -> #character.glyph_board_id.
