-module(shr_armor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   armor,
   {
      id :: id(),
      name :: binary(),
      omnimods :: shr_omnimods:type()
   }
).

-opaque type() :: #armor{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_name/1,
      get_omnimods/1
   ]
).

-export
(
   [
      default/0,
      default_id/0,
      from_id/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_id (type()) -> id().
get_id (Ar) -> Ar#armor.id.

-spec get_name (type()) -> binary().
get_name (Ar) -> Ar#armor.name.

-spec get_omnimods (type()) -> shr_omnimods:type().
get_omnimods (Ar) -> Ar#armor.omnimods.

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/armor/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/armor/basic.m4d)m4_dnl
from_id(_) ->
   default().

-spec default () -> type().
default () -> from_id(<<"0">>).

-spec default_id () -> id().
default_id () -> <<"0">>.
