-module(shr_attributes).

-include("tacticians/attributes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   attributes,
   {
      movement_points :: non_neg_integer(),
      health :: non_neg_integer(),
      dodge_chance :: integer(),
      parry_chance :: integer(),
      accuracy :: integer(),
      double_hit_chance :: integer(),
      critical_hit_chance :: integer(),
      damage_modifier :: integer()
   }
).

-opaque type() :: #attributes{}.
-type enum() ::
   (
      ?ATTRIBUTE_ACCURACY
      | ?ATTRIBUTE_CRITICAL_HIT_CHANCE
      | ?ATTRIBUTE_DAMAGE_MODIFIER
      | ?ATTRIBUTE_DODGE_CHANCE
      | ?ATTRIBUTE_DOUBLE_HIT_CHANCE
      | ?ATTRIBUTE_HEALTH
      | ?ATTRIBUTE_MOVEMENT_POINTS
      | ?ATTRIBUTE_PARRY_CHANCE
   ).

-type meta_enum() ::
   (
      enum()
      | ?ATTRIBUTE_ATTACK_SCORE
      | ?ATTRIBUTE_DEFENSE_SCORE
   ).

-export_type([type/0,enum/0,meta_enum/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_movement_points/1,
      get_health/1,
      get_dodge_chance/1,
      get_parry_chance/1,
      get_accuracy/1,
      get_double_hit_chance/1,
      get_critical_hit_chance/1,
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

-spec mod_dodge_chance (integer(), type()) -> type().
mod_dodge_chance (Mod, Atts) ->
   Atts#attributes{ dodge_chance = (Atts#attributes.dodge_chance + Mod) }.

-spec mod_parry_chance (integer(), type()) -> type().
mod_parry_chance (Mod, Atts) ->
   Atts#attributes{ parry_chance = (Atts#attributes.parry_chance + Mod) }.

-spec mod_accuracy (integer(), type()) -> type().
mod_accuracy (Mod, Atts) ->
   Atts#attributes{ accuracy = (Atts#attributes.accuracy + Mod) }.

-spec mod_double_hit_chance (integer(), type()) -> type().
mod_double_hit_chance (Mod, Atts) ->
   Atts#attributes
   {
      double_hit_chance = (Atts#attributes.double_hit_chance + Mod)
   }.

-spec mod_critical_hit_chance (integer(), type()) -> type().
mod_critical_hit_chance (Mod, Atts) ->
   Atts#attributes
   {
      critical_hit_chance = (Atts#attributes.critical_hit_chance + Mod)
   }.

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

-spec get_dodge_chance (type()) -> non_neg_integer().
get_dodge_chance (Atts) -> max(0, Atts#attributes.dodge_chance).

-spec get_parry_chance (type()) -> non_neg_integer().
get_parry_chance (Atts) -> max(0, Atts#attributes.parry_chance).

-spec get_accuracy (type()) -> non_neg_integer().
get_accuracy (Atts) -> max(0, Atts#attributes.accuracy).

-spec get_double_hit_chance (type()) -> non_neg_integer().
get_double_hit_chance (Atts) -> max(0, Atts#attributes.double_hit_chance).

-spec get_critical_hit_chance (type()) -> non_neg_integer().
get_critical_hit_chance (Atts) -> max(0, Atts#attributes.critical_hit_chance).

-spec get_damage_modifier (type()) -> non_neg_integer().
get_damage_modifier (Atts) -> max(0, Atts#attributes.damage_modifier).

-spec get_damage_multiplier (type()) -> float().
get_damage_multiplier (Atts) -> (get_damage_modifier(Atts) / 100).

-spec default () -> type().
default () ->
   #attributes
   {
      movement_points = ?ATTRIBUTE_MOVEMENT_POINTS_MIN,
      health = ?ATTRIBUTE_HEALTH_MIN,
      dodge_chance = ?ATTRIBUTE_DODGE_CHANCE_MIN,
      parry_chance = ?ATTRIBUTE_PARRY_CHANCE_MIN,
      accuracy = ?ATTRIBUTE_ACCURACY_MIN,
      double_hit_chance = ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MIN,
      critical_hit_chance = ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MIN,
      damage_modifier = ?ATTRIBUTE_DAMAGE_MODIFIER_MIN
   }.

-spec apply_mod (enum(), integer(), type()) -> type().
apply_mod(?ATTRIBUTE_HEALTH, Mod, Atts) -> mod_health(Mod, Atts);
apply_mod(?ATTRIBUTE_DODGE_CHANCE, Mod, Atts) -> mod_dodge_chance(Mod, Atts);
apply_mod(?ATTRIBUTE_PARRY_CHANCE, Mod, Atts) -> mod_parry_chance(Mod, Atts);
apply_mod(?ATTRIBUTE_ACCURACY, Mod, Atts) -> mod_accuracy(Mod, Atts);
apply_mod(?ATTRIBUTE_MOVEMENT_POINTS, Mod, Atts) ->
   mod_movement_points(Mod, Atts);
apply_mod(?ATTRIBUTE_DOUBLE_HIT_CHANCE, Mod, Atts) ->
   mod_double_hit_chance(Mod, Atts);
apply_mod(?ATTRIBUTE_CRITICAL_HIT_CHANCE, Mod, Atts) ->
   mod_critical_hit_chance(Mod, Atts);
apply_mod(?ATTRIBUTE_DAMAGE_MODIFIER, Mod, Atts) ->
   mod_damage_modifier(Mod, Atts).
