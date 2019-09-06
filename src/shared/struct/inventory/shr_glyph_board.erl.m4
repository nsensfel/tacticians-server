-module(shr_glyph_board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   glyph_board,
   {
      id :: id(),
      name :: binary(),
      slots :: list(integer())
   }
).

-type type() :: #glyph_board{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      from_id/1
   ]
).

-export
(
   [
      get_id/1,
      get_name/1,
      get_slots/1
   ]
).

-export
(
   [
      default/0,
      default_id/0,
      get_omnimods_with_glyphs/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_omnimods_with_glyphs_internals
   (
      shr_omnimods:type(),
      list(shr_glyph:type()),
      list(integer())
   )
   -> ('error' | {'ok', shr_omnimods:type()}).
get_omnimods_with_glyphs_internals (Omnimods, [], []) ->
   {ok, Omnimods};
get_omnimods_with_glyphs_internals (_Omnimods, [], _) ->
   error;
get_omnimods_with_glyphs_internals (_Omnimods, _, []) ->
   error;
get_omnimods_with_glyphs_internals (Omnimods, [Glyph|NextGlyphs], [M|NextMs]) ->
   Multiplier = (M / 100),
   GlyphOmnimods = shr_glyph:get_omnimods(Glyph),
   ModGlyphOmnimods = shr_omnimods:apply_coefficient(Multiplier, GlyphOmnimods),
   NextOmnimods = shr_omnimods:merge(Omnimods, ModGlyphOmnimods),

   get_omnimods_with_glyphs_internals(NextOmnimods, NextGlyphs, NextMs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_id (type()) -> id().
get_id (GlyphBoard) -> GlyphBoard#glyph_board.id.

-spec get_name (type()) -> binary().
get_name (GlyphBoard) -> GlyphBoard#glyph_board.name.

-spec get_slots (type()) -> list(non_neg_integer()).
get_slots (GlyphBoard) -> GlyphBoard#glyph_board.slots.

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/glyph_board/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/glyph_board/basic.m4d)m4_dnl
from_id(_) ->
   default().

-spec default () -> type().
default () -> from_id(<<"0">>).

-spec get_omnimods_with_glyphs
   (
      list(shr_glyph:type()),
      type()
   )
   -> shr_omnimods:type().
get_omnimods_with_glyphs (Glyphs, GlyphBoard) ->
   BoardSlots = GlyphBoard#glyph_board.slots,

   {ok, Omnimods} =
      get_omnimods_with_glyphs_internals
      (
         shr_omnimods:new(),
         Glyphs,
         BoardSlots
      ),

   Omnimods.

-spec default_id () -> id().
default_id () -> <<"0">>.
