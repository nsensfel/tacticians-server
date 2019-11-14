-module(btl_cond_heal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("tacticians/conditions.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      encode/1,
      get_turn_result_encoding/1,
      apply/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec heal_character
   (
      non_neg_integer(),
      btl_character:type(),
      non_neg_integer()
   )
   ->
   (
      {
         non_neg_integer(),
         btl_character:type(),
         btl_turn_result:type(),
         ataxic:basic()
      }
      | none
   ).
heal_character (ActorIX, S0Actor, S0HealingAmount) ->
   case btl_character:get_is_alive(S0Actor) of
      false -> none;
      true ->
         CurrentHealth = btl_character:get_current_health(S0Actor),
         BaseActor = btl_character:get_base_character(S0Actor),
         ActorAttributes = shr_character:get_attributes(BaseActor),
         MaxHealth = shr_attributes:get_maximum_health(ActorAttributes),
         MaxHealing = (MaxHealth - CurrentHealth),
         S1HealingAmount = min(MaxHealing, S0HealingAmount),
         {S1Actor, ActorAtaxicUpdate} =
            btl_character:ataxia_set_current_health
            (
               S0Actor,
               (CurrentHealth + S1HealingAmount)
            ),

         {
            S1HealingAmount,
            S1Actor,
            btl_turn_result:new_condition
            (
               btl_cond_heal,
               {ActorIX, S1HealingAmount}
            ),
            ActorAtaxicUpdate
         }
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply
   (
      btl_conditions:ref(),
      btl_character_turn_update:type(),
      shr_condition:context(any(), VolatileDataType),
   )
   -> {VolatileDataType, btl_character_turn_update:type()}.
apply (SelfRef, S0Update, S0Context) ->
   {_Trigger, _ReadOnlyData, VolatileData} = S0Context,

   {VolatileData, S0Update}.
