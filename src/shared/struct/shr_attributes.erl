-module(shr_attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   attributes,
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

-opaque type() :: #attributes{}.

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
mod_movement_points (Mod, Atts) ->
   Atts#attributes
   {
      movement_points = (Atts#attributes.movement_points + Mod)
   }.

-spec mod_health (integer(), type()) -> type().
mod_health (Mod, Atts) ->
   Atts#attributes{ health = (Atts#attributes.health + Mod) }.

-spec mod_dodges (integer(), type()) -> type().
mod_dodges (Mod, Atts) ->
   Atts#attributes{ dodges = (Atts#attributes.dodges + Mod) }.

-spec mod_parries (integer(), type()) -> type().
mod_parries (Mod, Atts) ->
   Atts#attributes{ parries = (Atts#attributes.parries + Mod) }.

-spec mod_accuracy (integer(), type()) -> type().
mod_accuracy (Mod, Atts) ->
   Atts#attributes{ accuracy = (Atts#attributes.accuracy + Mod) }.

-spec mod_double_hits (integer(), type()) -> type().
mod_double_hits (Mod, Atts) ->
   Atts#attributes{ double_hits = (Atts#attributes.double_hits + Mod) }.

-spec mod_critical_hits (integer(), type()) -> type().
mod_critical_hits (Mod, Atts) ->
   Atts#attributes{ critical_hits = (Atts#attributes.critical_hits + Mod) }.

-spec mod_damage_modifier (integer(), type()) -> type().
mod_damage_modifier (Mod, Atts) ->
   Atts#attributes
   {
      damage_modifier = (Atts#attributes.damage_modifier + Mod)
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_movement_points (type()) -> non_neg_integer().
get_movement_points (Atts) -> max(0, Atts#attributes.movement_points).

-spec get_health (type()) -> non_neg_integer().
get_health (Atts) -> max(1, Atts#attributes.health).

-spec get_dodges (type()) -> non_neg_integer().
get_dodges (Atts) -> max(0, Atts#attributes.dodges).

-spec get_parries (type()) -> non_neg_integer().
get_parries (Atts) -> max(0, Atts#attributes.parries).

-spec get_accuracy (type()) -> non_neg_integer().
get_accuracy (Atts) -> max(0, Atts#attributes.accuracy).

-spec get_double_hits (type()) -> non_neg_integer().
get_double_hits (Atts) -> max(0, Atts#attributes.double_hits).

-spec get_critical_hits (type()) -> non_neg_integer().
get_critical_hits (Atts) -> max(0, Atts#attributes.critical_hits).

-spec get_damage_modifier (type()) -> non_neg_integer().
get_damage_modifier (Atts) -> max(0, Atts#attributes.damage_modifier).

-spec get_damage_multiplier (type()) -> float().
get_damage_multiplier (Atts) -> (get_damage_modifier(Atts) / 100).

-spec default () -> type().
default () ->
   #attributes
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
apply_mod(mheal, Value, Atts) -> mod_health(Value, Atts);
apply_mod(mpts, Value, Atts) -> mod_movement_points(Value, Atts);
apply_mod(dodg, Value, Atts) -> mod_dodges(Value, Atts);
apply_mod(pary, Value, Atts) -> mod_parries(Value, Atts);
apply_mod(accu, Value, Atts) -> mod_accuracy(Value, Atts);
apply_mod(dhit, Value, Atts) -> mod_double_hits(Value, Atts);
apply_mod(crit, Value, Atts) -> mod_critical_hits(Value, Atts);
apply_mod(dmgm, Value, Atts) -> mod_damage_modifier(Value, Atts).
