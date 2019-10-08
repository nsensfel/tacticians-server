-module(btl_cond_heal).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
      btl_condition:update_order(),
      [{btl_character:type(), ataxic:basic()}]
   }.
apply_to_character (Condition, S0Character) ->
   {_, Amount} = btl_condition:get_parameters(Condition),

   case btl_character:get_is_alive(S0Character) of
      false -> {Condition, btl_condition:do_nothing(), []};
      true ->
         RemainingUses = btl_condition:get_remaining_uses(Condition),
         CurrentHealth = btl_character:get_current_health(S0Character),
         MaxHealth =
            shr_attributes:get_health
            (
               shr_character:get_attributes
               j(
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
                  btl_condition:do_nothing(),
                  [{S1Character, CharacterUpdate}]
               };

            (RemainingUses == 1) ->
               {
                  btl_condition:set_remaining_uses
                  (
                     UpdatedRemainingUses,
                     Condition
                  ),
                  btl_condition:remove(),
                  [{S1Character, CharacterUpdate}]
               };

            (RemainingUses == 0) ->
               {
                  Condition,
                  btl_condition:remove(),
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
                  btl_condition:update(ConditionUpdate),
                  [{S1Character, CharacterUpdate}]
               }
         end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply
   (
      btl_condition:trigger(),
      tuple(),
      btl_condition:type(),
      btl_character_turn_update:type()
   ) ->
   {
      [{btl_condition:type(), ataxic:basic()}],
      tuple(),
      btl_character_turn_update:type()
   }.
apply(?CONDITION_TRIGGER_START_OF_PLAYER_TURN, {PlayerIX}, Condition, Update) ->
apply(?CONDITION_TRIGGER_END_OF_PLAYER_TURN, {PlayerIX}, Condition, Update) ->
apply
(
   ?CONDITION_TRIGGER_START_OF_CHARACTER_TURN,
   {CharacterIX},
   Condition,
   Update
) ->
apply
(
   ?CONDITION_TRIGGER_END_OF_CHARACTER_TURN,
   {CharacterIX},
   Condition,
   Update
) ->
apply
(
   ?CONDITION_TRIGGER_END_OF_CHARACTER_TURN,
   {CharacterIX},
   Condition,
   Update
) ->

   Trigger,
   Parameters,
   Condition,
   Update) ->
   {TargetIX, Amount} =
      case btl_condition:get_parameters(Condition) of
         {StoredTargetIX, StoredAmount} -> {StoredTargetIX, StoredAmount};
         Other -> error({condition, parameter, Other})
      end,

   % TODO
   {[{Condition, []}], Update}.
