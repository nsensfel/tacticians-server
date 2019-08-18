-module(blc_attribute).

-include("tacticians/attributes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_info/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_info
   (
      shr_attributes:meta_enum()
   )
   ->
   {
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer()
   }.
get_info (?ATTRIBUTE_ACCURACY) ->
   {
      ?ATTRIBUTE_ACCURACY_MIN,
      ?ATTRIBUTE_ACCURACY_DEFAULT,
      ?ATTRIBUTE_ACCURACY_MAX,
      ?ATTRIBUTE_ACCURACY_COST
   };
get_info (?ATTRIBUTE_ATTACK_SCORE) ->
   {
      ?ATTRIBUTE_ATTACK_SCORE_MIN,
      ?ATTRIBUTE_ATTACK_SCORE_DEFAULT,
      ?ATTRIBUTE_ATTACK_SCORE_MAX,
      ?ATTRIBUTE_ATTACK_SCORE_COST
   };
get_info (?ATTRIBUTE_CRITICAL_HIT_CHANCE) ->
   {
      ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MIN,
      ?ATTRIBUTE_CRITICAL_HIT_CHANCE_DEFAULT,
      ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MAX,
      ?ATTRIBUTE_CRITICAL_HIT_CHANCE_COST
   };
get_info (?ATTRIBUTE_DEFENSE_SCORE) ->
   {
      ?ATTRIBUTE_DEFENSE_SCORE_MIN,
      ?ATTRIBUTE_DEFENSE_SCORE_DEFAULT,
      ?ATTRIBUTE_DEFENSE_SCORE_MAX,
      ?ATTRIBUTE_DEFENSE_SCORE_COST
   };
get_info (?ATTRIBUTE_DODGE_CHANCE) ->
   {
      ?ATTRIBUTE_DODGE_CHANCE_MIN,
      ?ATTRIBUTE_DODGE_CHANCE_DEFAULT,
      ?ATTRIBUTE_DODGE_CHANCE_MAX,
      ?ATTRIBUTE_DODGE_CHANCE_COST
   };
get_info (?ATTRIBUTE_DOUBLE_HIT_CHANCE) ->
   {
      ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MIN,
      ?ATTRIBUTE_DOUBLE_HIT_CHANCE_DEFAULT,
      ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MAX,
      ?ATTRIBUTE_DOUBLE_HIT_CHANCE_COST
   };
get_info (?ATTRIBUTE_HEALTH) ->
   {
      ?ATTRIBUTE_HEALTH_MIN,
      ?ATTRIBUTE_HEALTH_DEFAULT,
      ?ATTRIBUTE_HEALTH_MAX,
      ?ATTRIBUTE_HEALTH_COST
   };
get_info (?ATTRIBUTE_MOVEMENT_POINTS) ->
   {
      ?ATTRIBUTE_MOVEMENT_POINTS_MIN,
      ?ATTRIBUTE_MOVEMENT_POINTS_DEFAULT,
      ?ATTRIBUTE_MOVEMENT_POINTS_MAX,
      ?ATTRIBUTE_MOVEMENT_POINTS_COST
   };
get_info (?ATTRIBUTE_PARRY_CHANCE) ->
   {
      ?ATTRIBUTE_PARRY_CHANCE_MIN,
      ?ATTRIBUTE_PARRY_CHANCE_DEFAULT,
      ?ATTRIBUTE_PARRY_CHANCE_MAX,
      ?ATTRIBUTE_PARRY_CHANCE_COST
   }.
