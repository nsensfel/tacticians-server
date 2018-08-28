-module(shr_weapon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: non_neg_integer().

-record
(
   weapon,
   {
      id :: id(),
      name :: binary(),
      range_min:: non_neg_integer(),
      range_max :: non_neg_integer(),
      omnimods :: shr_omnimods:type()
   }
).

-opaque type() :: #weapon{}.

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
      get_minimum_range/1,
      get_maximum_range/1,
      get_omnimods/1
   ]
).

-export
(
   [
      random_id/0,
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
get_id (Wp) -> Wp#weapon.id.

-spec get_name (type()) -> binary().
get_name (Wp) -> Wp#weapon.name.

-spec get_minimum_range (type()) -> non_neg_integer().
get_minimum_range (Wp) -> Wp#weapon.range_min.

-spec get_maximum_range (type()) -> non_neg_integer().
get_maximum_range (Wp) -> Wp#weapon.range_max.

-spec get_omnimods (type()) -> shr_omnimods:type().
get_omnimods (Wp) -> Wp#weapon.omnimods.

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/weapon/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/weapon/basic.m4d)m4_dnl
from_id (_) ->
   from_id(0).

-spec random_id () -> id().
random_id () -> shr_roll:between(0, 24).
