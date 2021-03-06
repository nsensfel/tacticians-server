-module(shr_glyph).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   glyph,
   {
      id :: id(),
      family_id :: id(),
      name :: binary(),
      omnimods :: shr_omnimods:type()
   }
).

-type type() :: #glyph{}.

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
      get_family_id/1,
      get_omnimods/1
   ]
).

-export
(
   [
      default/0,
      default_id/0,
      default_family_id/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_id (type()) -> id().
get_id (Glyph) -> Glyph#glyph.id.

-spec get_family_id (type()) -> id().
get_family_id (Glyph) -> Glyph#glyph.family_id.

-spec get_name (type()) -> binary().
get_name (Glyph) -> Glyph#glyph.name.

-spec get_omnimods (type()) -> shr_omnimods:type().
get_omnimods (Glyph) -> Glyph#glyph.omnimods.

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/glyph/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/glyph/basic.m4d)m4_dnl
from_id(_) ->
   from_id(<<"0">>).

-spec default () -> type().
default () -> from_id(<<"0">>).

-spec default_id () -> id().
default_id () -> <<"0">>.

-spec default_family_id () -> id().
default_family_id () -> <<"0">>.
