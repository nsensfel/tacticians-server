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
      dodges :: non_neg_integer(),
      parries :: non_neg_integer(),
      accuracy :: non_neg_integer(),
      double_hits :: non_neg_integer(),
      critical_hits :: non_neg_integer(),
      damage_modifier :: float()
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

      apply_mod/3
   ]
).

-export
(
   [
      new_raw/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec float_to_int (float()) -> integer().
float_to_int (F) ->
   I = trunc(F),
   case (F > I) of
      true -> (I + 1);
      _ -> I
   end.

-spec min_max (number(), number(), number()) -> number().
min_max (Min, Max, V) -> min(Max, max(Min, V)).

-spec average (list(number())) -> number().
%average ([]) -> 0;
average (L) -> lists:sum(L) / length(L).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 004 | 023 | 058 | 104 | 200 |
-spec gentle_squared_growth (number()) -> non_neg_integer().
gentle_squared_growth (V) -> float_to_int(math:pow(V, 1.8) / 20.0).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 001 | 005 | 018 | 041 | 100 |
-spec sudden_squared_growth (number()) -> non_neg_integer().
sudden_squared_growth (V) -> float_to_int(math:pow(V, 2.5) / 1000.0).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 002 | 006 | 016 | 049 | 256 |
-spec sudden_exp_growth (number()) -> non_neg_integer().
sudden_exp_growth (V) -> float_to_int(math:pow(4.0, V / 25.0)).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 040 | 066 | 079 | 088 | 099 |
% Seems too generous, values for attributes below 50 should dip faster and
% lower.
%-spec already_high_slow_growth (non_neg_integer()) -> non_neg_integer().
%already_high_slow_growth (V) -> float_to_int(30 * math:log((V + 5)/4)).

-spec damage_base_modifier (non_neg_integer()) -> float().
damage_base_modifier (Strength) -> (math:pow((Strength + 10)*4, 1.5) / 3000.0).

-spec make_movement_points_safe (non_neg_integer()) -> non_neg_integer().
make_movement_points_safe (Val) -> min_max(0, 200, Val).

-spec make_health_safe (non_neg_integer()) -> non_neg_integer().
make_health_safe (Val) -> max(1, Val).

-spec make_dodges_safe (non_neg_integer()) -> non_neg_integer().
make_dodges_safe (Val) -> min_max(0, 100, Val).

-spec make_parries_safe (non_neg_integer()) -> non_neg_integer().
make_parries_safe (Val) -> min_max(0, 75, Val).

-spec make_accuracy_safe (non_neg_integer()) -> non_neg_integer().
make_accuracy_safe (Val) -> min_max(0, 100, Val).

-spec make_double_hits_safe (non_neg_integer()) -> non_neg_integer().
make_double_hits_safe (Val) -> min_max(0, 100, Val).

-spec make_critical_hits_safe (non_neg_integer()) -> non_neg_integer().
make_critical_hits_safe (Val) -> min_max(0, 100, Val).

-spec mod_movement_points (integer(), type()) -> type().
mod_movement_points (Mod, Stats) ->
   Stats#statistics
   {
      movement_points =
         make_movement_points_safe(get_movement_points(Stats) + Mod)
   }.

-spec mod_health (integer(), type()) -> type().
mod_health (Mod, Stats) ->
   Stats#statistics
   {
      health = make_health_safe(get_health(Stats) + Mod)
   }.

-spec mod_dodges (integer(), type()) -> type().
mod_dodges (Mod, Stats) ->
   Stats#statistics
   {
      dodges = make_dodges_safe(get_dodges(Stats) + Mod)
   }.

-spec mod_parries (integer(), type()) -> type().
mod_parries (Mod, Stats) ->
   Stats#statistics
   {
      parries = make_parries_safe(get_parries(Stats) + Mod)
   }.

-spec mod_accuracy (integer(), type()) -> type().
mod_accuracy (Mod, Stats) ->
   Stats#statistics
   {
      accuracy = make_accuracy_safe(get_accuracy(Stats) + Mod)
   }.

-spec mod_double_hits (integer(), type()) -> type().
mod_double_hits (Mod, Stats) ->
   Stats#statistics
   {
      double_hits = make_double_hits_safe(get_double_hits(Stats) + Mod)
   }.

-spec mod_critical_hits (integer(), type()) -> type().
mod_critical_hits (Mod, Stats) ->
   Stats#statistics
   {
      critical_hits = make_critical_hits_safe(get_critical_hits(Stats) + Mod)
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_movement_points (type()) -> non_neg_integer().
get_movement_points (Stats) -> Stats#statistics.movement_points.

-spec get_health (type()) -> non_neg_integer().
get_health (Stats) -> Stats#statistics.health.

-spec get_dodges (type()) -> non_neg_integer().
get_dodges (Stats) -> Stats#statistics.dodges.

-spec get_parries (type()) -> non_neg_integer().
get_parries (Stats) -> Stats#statistics.parries.

-spec get_accuracy (type()) -> non_neg_integer().
get_accuracy (Stats) -> Stats#statistics.accuracy.

-spec get_double_hits (type()) -> non_neg_integer().
get_double_hits (Stats) -> Stats#statistics.double_hits.

-spec get_critical_hits (type()) -> non_neg_integer().
get_critical_hits (Stats) -> Stats#statistics.critical_hits.

-spec get_damage_modifier (type()) -> float().
get_damage_modifier (Stats) -> Stats#statistics.damage_modifier.

-spec new_raw (shr_attributes:type()) -> type().
new_raw (Attributes) ->
   Constitution = shr_attributes:get_constitution(Attributes),
   Dexterity = shr_attributes:get_dexterity(Attributes),
   Intelligence = shr_attributes:get_intelligence(Attributes),
   Mind = shr_attributes:get_mind(Attributes),
   Speed = shr_attributes:get_speed(Attributes),
   Strength = shr_attributes:get_strength(Attributes),

   #statistics
   {
      movement_points =
         gentle_squared_growth
         (
            average([Mind, Constitution, Constitution, Speed, Speed, Speed])
         ),
      health =
         gentle_squared_growth
         (
            average([Constitution, Constitution, Constitution, Mind])
         ),
      dodges =
         sudden_exp_growth(average([Dexterity, Mind, Speed])),
      parries =
         sudden_exp_growth
         (
            average([Dexterity, Intelligence, Speed, Strength])
         ),
      accuracy = sudden_squared_growth(Dexterity),
      double_hits = sudden_squared_growth(average([Mind, Speed])),
      critical_hits = sudden_squared_growth(Intelligence),
      damage_modifier = damage_base_modifier(Strength)
   }.

-spec apply_mod (atom(), integer(), type()) -> type().
apply_mod(mheal, Value, Stats) -> mod_health(Value, Stats);
apply_mod(mpts, Value, Stats) -> mod_movement_points(Value, Stats);
apply_mod(dodg, Value, Stats) -> mod_dodges(Value, Stats);
apply_mod(pary, Value, Stats) -> mod_parries(Value, Stats);
apply_mod(accu, Value, Stats) -> mod_accuracy(Value, Stats);
apply_mod(dhit, Value, Stats) -> mod_double_hits(Value, Stats);
apply_mod(crit, Value, Stats) -> mod_critical_hits(Value, Stats).
