-module(rst_character).

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
      decode/1,
      random/0
   ]
).

-export
(
   [
      validate/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec validate_name (binary()) -> ok.
validate_name (_Name) ->
   % TODO [SECURITY][LOW]: unimplemented
   ok.

-spec validate_portrait (shr_inventory:type(), binary()) -> ok.
validate_portrait (_Inventory, _Portrait) ->
   % TODO [SECURITY][LOW]: unimplemented
   ok.

-spec validate_weapons
   (
      shr_inventory:type(),
      {shr_weapon:id(), shr_weapon:id()}
   )
   -> ok.
validate_weapons (_Inventory, {_ActiveWeapon, _SecondaryWeapon}) ->
   % TODO [SECURITY][LOW]: unimplemented
   ok.

-spec validate_armor (shr_inventory:type(), shr_armor:id()) -> ok.
validate_armor (_Inventory, _Armor) ->
   % TODO [SECURITY][LOW]: unimplemented
   ok.

-spec validate_glyphs (shr_inventory:type(), array:array(shr_glyph:id())) -> ok.
validate_glyphs (_Inventory, _Glyphs) ->
   % TODO [SECURITY][LOW]: unimplemented
   ok.

-spec validate_glyph_board (shr_inventory:type(), shr_glyph_board:id()) -> ok.
validate_glyph_board (_Inventory, _GlyphBoard) ->
   % TODO [SECURITY][LOW]: unimplemented
   ok.

-spec validate_glyphs_on_board
   (
      array:array(shr_glyph:id()),
      shr_glyph_board:id()
   )
   -> ok.
validate_glyphs_on_board (_Glyphs, _GlyphBoard) ->
   % TODO [SECURITY][LOW]: unimplemented
   ok.

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

-spec decode (map()) -> type().
decode (JSONReqMap) ->
   Name = maps:get(<<"nam">>, JSONReqMap),
   Portrait = maps:get(<<"prt">>, JSONReqMap),
   ActiveWeapon = maps:get(<<"awp">>, JSONReqMap),
   SecondaryWeapon = maps:get(<<"swp">>, JSONReqMap),
   Armor = maps:get(<<"ar">>, JSONReqMap),
   GlyphsList = maps:get(<<"gls">>, JSONReqMap),
   GlyphBoard = maps:get(<<"gb">>, JSONReqMap),

   #character
   {
      name = Name,
      portrait = Portrait,
      weapon_ids = {ActiveWeapon, SecondaryWeapon},
      armor_id = Armor,
      glyph_ids = array:from_list(GlyphsList),
      glyph_board_id = GlyphBoard
   }.

-spec validate (shr_inventory:type(), type()) -> ok.
validate (Inventory, Character) ->
   Glyphs = Character#character.glyph_ids,
   GlyphBoard = Character#character.glyph_board_id,

   validate_name(Character#character.name),
   validate_portrait(Inventory, Character#character.portrait),
   validate_weapons(Inventory, Character#character.weapon_ids),
   validate_armor(Inventory, Character#character.armor_id),
   validate_glyphs(Inventory, Glyphs),
   validate_glyph_board(Inventory, GlyphBoard),
   validate_glyphs_on_board(Glyphs, GlyphBoard),

   ok.
