-module(shr_skill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   skill,
   {
      id :: id(),
      module :: atom(),
      name :: binary(),
      cost :: non_neg_integer(),
      reserve :: non_neg_integer(),
      targets :: (non_neg_integer() | -1),
      locations :: (non_neg_integer() | -1),
      duration :: (non_neg_integer() | -1),
      uses :: (non_neg_integer() | -1),
      chance :: (0..100 | -1),
      power :: (non_neg_integer() | -1),
      range :: (non_neg_integer() | -1)
   }
).

-opaque type() :: #skill{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_module/1,
      get_name/1,
      get_cost/1,
      get_reserve/1,
      get_targets/1,
      get_locations/1,
      get_duration/1,
      get_uses/1,
      get_chance/1,
      get_power/1,
      get_range/1
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

-export
(
   [
      encode/1
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
get_id (Skill) -> Skill#skill.id.

-spec get_module (type()) -> atom().
get_module (Skill) -> Skill#skill.module.

-spec get_name (type()) -> binary().
get_name (Skill) -> Skill#skill.name.

-spec get_cost (type()) -> non_neg_integer().
get_cost (Skill) -> Skill#skill.cost.

-spec get_reserve (type()) -> non_neg_integer().
get_reserve (Skill) -> Skill#skill.reserve.

-spec get_targets (type()) -> (non_neg_integer() | -1).
get_targets (Skill) -> Skill#skill.targets.

-spec get_locations (type()) -> (non_neg_integer() | -1).
get_locations (Skill) -> Skill#skill.locations.

-spec get_duration (type()) -> (non_neg_integer() | -1).
get_duration (Skill) -> Skill#skill.duration.

-spec get_uses (type()) -> (non_neg_integer() | -1).
get_uses (Skill) -> Skill#skill.uses.

-spec get_power (type()) -> (non_neg_integer() | -1).
get_power (Skill) -> Skill#skill.power.

-spec get_chance (type()) -> (0..100 | -1).
get_chance (Skill) -> Skill#skill.chance.

-spec get_range (type()) -> (non_neg_integer() | -1).
get_range (Skill) -> Skill#skill.range.

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/skill/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/skill/basic.m4d)m4_dnl
from_id (_) ->
   default().

-spec default () -> type().
default () -> from_id(<<"0">>).

-spec default_id () -> id().
default_id () -> <<"0">>.

-spec encode (type()) -> {list({binary(), any()})}.
encode (Skill) ->
   {
      [
         { <<"id">>, Skill#skill.id },
         { <<"nam">>, Skill#skill.name },
         { <<"cos">>, Skill#skill.cost },
         { <<"res">>, Skill#skill.reserve },
         { <<"tar">>, Skill#skill.targets },
         { <<"loc">>, Skill#skill.locations },
         { <<"dur">>, Skill#skill.duration },
         { <<"use">>, Skill#skill.uses },
         { <<"cha">>, Skill#skill.chance },
         { <<"pow">>, Skill#skill.power },
         { <<"ran">>, Skill#skill.range }
      ]
   }.
