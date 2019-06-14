-module(shr_statistics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   statistics,
   {
      movement_points :: non_neg_integer(),
      health :: non_neg_integer(),
      dodges :: integer(),
      parries :: integer(),
      accuracy :: integer(),
      double_hits :: integer(),
      critical_hits :: integer(),
      damage_modifier :: integer()
   }
).

-opaque type() :: #statistics{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_movement_points/1,
      get_health/1,
      get_dodges/1,
      get_parries/1,
      get_accuracy/1,
      get_double_hits/1,
      get_critical_hits/1,
      get_damage_modifier/1,
      get_damage_multiplier/1,

      apply_mod/3
   ]
).

-export
(
   [
      default/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec mod_movement_points (integer(), type()) -> type().
mod_movement_points (Mod, Stats) ->
   Stats#statistics
   {
      movement_points = (Stats#statistics.movement_points + Mod)
   }.

-spec mod_health (integer(), type()) -> type().
mod_health (Mod, Stats) ->
   Stats#statistics{ health = (Stats#statistics.health + Mod) }.

-spec mod_dodges (integer(), type()) -> type().
mod_dodges (Mod, Stats) ->
   Stats#statistics{ dodges = (Stats#statistics.dodges + Mod) }.

-spec mod_parries (integer(), type()) -> type().
mod_parries (Mod, Stats) ->
   Stats#statistics{ parries = (Stats#statistics.parries + Mod) }.

-spec mod_accuracy (integer(), type()) -> type().
mod_accuracy (Mod, Stats) ->
   Stats#statistics{ accuracy = (Stats#statistics.accuracy + Mod) }.

-spec mod_double_hits (integer(), type()) -> type().
mod_double_hits (Mod, Stats) ->
   Stats#statistics{ double_hits = (Stats#statistics.double_hits + Mod) }.

-spec mod_critical_hits (integer(), type()) -> type().
mod_critical_hits (Mod, Stats) ->
   Stats#statistics{ critical_hits = (Stats#statistics.critical_hits + Mod) }.

-spec mod_damage_modifier (integer(), type()) -> type().
mod_damage_modifier (Mod, Stats) ->
   Stats#statistics
   {
      damage_modifier = (Stats#statistics.damage_modifier + Mod)
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_movement_points (type()) -> non_neg_integer().
get_movement_points (Stats) -> max(0, Stats#statistics.movement_points).

-spec get_health (type()) -> non_neg_integer().
get_health (Stats) -> max(1, Stats#statistics.health).

-spec get_dodges (type()) -> non_neg_integer().
get_dodges (Stats) -> max(0, Stats#statistics.dodges).

-spec get_parries (type()) -> non_neg_integer().
get_parries (Stats) -> max(0, Stats#statistics.parries).

-spec get_accuracy (type()) -> non_neg_integer().
get_accuracy (Stats) -> max(0, Stats#statistics.accuracy).

-spec get_double_hits (type()) -> non_neg_integer().
get_double_hits (Stats) -> max(0, Stats#statistics.double_hits).

-spec get_critical_hits (type()) -> non_neg_integer().
get_critical_hits (Stats) -> max(0, Stats#statistics.critical_hits).

-spec get_damage_modifier (type()) -> non_neg_integer().
get_damage_modifier (Stats) -> max(0, Stats#statistics.damage_modifier).

-spec get_damage_multiplier (type()) -> float().
get_damage_multiplier (Stats) -> (get_damage_modifier(Stats) / 100).

-spec default () -> type().
default () ->
   #statistics
   {
      movement_points = 0,
      health = 1,
      dodges = 0,
      parries = 0,
      accuracy = 0,
      double_hits = 0,
      critical_hits = 0,
      damage_modifier = 100
   }.

-spec apply_mod (atom(), integer(), type()) -> type().
apply_mod(mheal, Value, Stats) -> mod_health(Value, Stats);
apply_mod(mpts, Value, Stats) -> mod_movement_points(Value, Stats);
apply_mod(dodg, Value, Stats) -> mod_dodges(Value, Stats);
apply_mod(pary, Value, Stats) -> mod_parries(Value, Stats);
apply_mod(accu, Value, Stats) -> mod_accuracy(Value, Stats);
apply_mod(dhit, Value, Stats) -> mod_double_hits(Value, Stats);
apply_mod(crit, Value, Stats) -> mod_critical_hits(Value, Stats);
apply_mod(dmgm, Value, Stats) -> mod_damage_modifier(Value, Stats).
