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
                  do_nothing,
                  [{S1Character, CharacterUpdate}]
               };

            (RemainingUses == 1) ->
               {
                  btl_condition:set_remaining_uses
                  (
                     UpdatedRemainingUses,
                     Condition
                  ),
                  remove,
                  [{S1Character, CharacterUpdate}]
               };

            (RemainingUses == 0) ->
               {
                  Condition,
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

-spec handle_trigger
   (
      btl_condition:trigger(),
      btl_condition:type()
   )
   -> btl_condition:trigger().
handle_trigger ({TriggerType, S0TriggerData}, Condition) ->
   {TargetIX, _Amount} = btl_condition:get_parameters(Condition),

   case
      (
         (TriggerType == ?CONDITION_TRIGGER_START_OF_OWN_ATTACK)
         or (TriggerType == ?CONDITION_TRIGGER_END_OF_OWN_ATTACK)
         or (TriggerType == ?CONDITION_TRIGGER_START_OF_OWN_HIT)
         or (TriggerType == ?CONDITION_TRIGGER_END_OF_OWN_HIT)
         or (TriggerType == ?CONDITION_TRIGGER_OWN_DODGE)
         or (TriggerType == ?CONDITION_TRIGGER_OWN_CRITICAL)
         or (TriggerType == ?CONDITION_TRIGGER_OWN_DOUBLE_HIT)
         or (TriggerType == ?CONDITION_TRIGGER_OWN_DAMAGE)
         or (TriggerType == ?CONDITION_TRIGGER_START_OF_TARGET_ATTACK)
         or (TriggerType == ?CONDITION_TRIGGER_END_OF_TARGET_ATTACK)
         or (TriggerType == ?CONDITION_TRIGGER_START_OF_TARGET_HIT)
         or (TriggerType == ?CONDITION_TRIGGER_END_OF_TARGET_HIT)
         or (TriggerType == ?CONDITION_TRIGGER_TARGET_DODGE)
         or (TriggerType == ?CONDITION_TRIGGER_TARGET_CRITICAL)
         or (TriggerType == ?CONDITION_TRIGGER_TARGET_DOUBLE_HIT)
         or (TriggerType == ?CONDITION_TRIGGER_TARGET_DAMAGE)
      )
   of
      false -> {TriggerType, S0TriggerData};
      true ->
         {Char0IX, Char0, Char1IX, Char1} = TriggerData,
         if
            (Char0IX == TargetIX) ->
               {_UpdatedCondition, _UpdateOrder, UpdatedChar} =
                  apply_to_character(Condition, Char0),

               {
                  TriggerType,
                  {
                     Char0IX
                     UpdatedChar,
                     Char1IX,
                     Char1
                  }
               };

            (Char1IX == TargetIX) ->
               {_UpdatedCondition, _UpdateOrder, UpdatedChar} =
                  apply_to_character(Condition, Char0),

               {
                  TriggerType,
                  {
                     Char0IX
                     Char0,
                     Char1IX,
                     UpdatedChar
                  }
               };

            true -> {TriggerType, S0TriggerData}
         end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply
   (
      btl_condition:trigger(),
      btl_condition:type(),
      btl_character_turn_update:type()
   ) ->
   {
      btl_condition:type(),
      btl_condition:update_action(),
      btl_condition:trigger(),
      btl_character_turn_update:type()
   }.
apply (S0Trigger, Condition, Update) ->
   S1Trigger = handle_trigger(S0Trigger, Condition),

   Parameters,
   Condition,
   Update) ->
   {TargetIX, Amount} =
      case btl_condition:get_parameters(Condition) of
         {StoredTargetIX, StoredAmount} -> {StoredTargetIX, StoredAmount};
         Other -> error({condition, parameter, Other})
      end,
   {[{Condition, []}], Update}.
