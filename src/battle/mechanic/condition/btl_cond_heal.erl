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
      apply/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_to_character
   (
      btl_condition:type(),
      btl_character:type()
   )
   ->
   {
      btl_condition:type(),
      btl_condition:update_action(),
      [{btl_character:type(), ataxic:basic()}]
   }.
apply_to_character (Condition, S0Character) ->
   {_TargetIX, Amount} = btl_condition:get_parameters(Condition),

   case btl_character:get_is_alive(S0Character) of
      false -> {Condition, none, []};
      true ->
         RemainingUses = btl_condition:get_remaining_uses(Condition),
         CurrentHealth = btl_character:get_current_health(S0Character),
         MaxHealth =
            shr_attributes:get_health
            (
               shr_character:get_attributes
               (
                  btl_character:get_base_character(S0Character)
               )
            ),

         UpdatedHealth = min(MaxHealth, (CurrentHealth + Amount)),
         UpdatedRemainingUses =

         {S1Character, CharacterUpdate} =
            btl_character:ataxia_set_current_health(UpdatedHealth, S0Character),

         if
            (RemainingUses == -1) ->
               {
                  Condition,
                  none,
                  [{S1Character, CharacterUpdate}]
               };

            (RemainingUses == 1) ->
               {
                  remove,
                  [{S1Character, CharacterUpdate}]
               };

            (RemainingUses == 0) ->
               {
                  remove,
                  [{S1Character, CharacterUpdate}]
               };

            true ->
               {UpdatedCondition, ConditionUpdate} =
                  btl_condition:ataxia_set_remaining_uses
                  (
                     UpdatedRemainingUses,
                     Condition
                  ),
               {
                  UpdatedCondition,
                  {update, ConditionUpdate},
                  [{S1Character, CharacterUpdate}]
               }
         end
   end.

-spec handle_context
   (
      shr_condition:context(),
      btl_condition:type()
   )
   -> shr_condition:context().
handle_context ({Trigger, ReadOnly, VolatileData}, Condition) ->
   {_TargetIX, _Amount} = btl_condition:get_parameters(Condition),
   {Trigger, ReadOnly, VolatileData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply
   (
      shr_condition:context(any(), VolatileDataType),
      btl_character_turn_update:type(),
      btl_condition:type()
   ) ->
   {
      VolatileDataType,
      btl_character_turn_update:type(),
      btl_condition:update_action()
   }.
apply (S0Context, S0Update, _S0Condition) ->
   {_Trigger, _ReadOnlyData, VolatileData} = S0Context,

   {VolatileData, S0Update, none}.
