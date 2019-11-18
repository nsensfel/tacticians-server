-module(btl_cond_heal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("tacticians/conditions.hrl").

-type turn_result() :: {non_neg_integer(), non_neg_integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([turn_result/0]).

-export
(
   [
      encode_turn_result/1,
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
         MaxHealth = shr_attributes:get_health(ActorAttributes),
         MaxHealing = (MaxHealth - CurrentHealth),
         S1HealingAmount = min(MaxHealing, S0HealingAmount),
         {S1Actor, ActorAtaxicUpdate} =
            btl_character:ataxia_set_current_health
            (
               (CurrentHealth + S1HealingAmount),
               S0Actor
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

-spec perform_on_target
   (
      non_neg_integer(),
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
perform_on_target (TargetIX, Power, S0Update) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),
   {S0Target, S1Battle} = btl_battle:get_resolved_character(TargetIX, S0Battle),

   case heal_character(TargetIX, S0Target, Power) of
      none ->
         S1Update = btl_character_turn_update:set_battle(S1Battle, S0Update),
         S1Update;

      {
         _HealingAmount,
         S1Target,
         TurnResult,
         TargetAtaxicUpdate
      }
         ->
            {S2Battle, BattleAtaxicUpdate} =
               btl_battle:ataxia_set_character
               (
                  TargetIX,
                  S1Target,
                  TargetAtaxicUpdate,
                  S1Battle
               ),

            S1Update =
               btl_character_turn_update:ataxia_set_battle
               (
                  S2Battle,
                  BattleAtaxicUpdate,
                  S0Update
               ),

            S2Update =
               btl_character_turn_update:add_to_timeline(TurnResult, S1Update),

            S2Update
   end.

-spec perform_on_location
   (
      shr_location:type(),
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
perform_on_location (Location, Power, Update) ->
   Battle = btl_character_turn_update:get_battle(Update),
   Characters = btl_battle:get_characters(Battle),

   MaybeResultIX =
      orddict:fold
      (
         fun (IX, Char, CurrentResult) ->
            case CurrentResult of
               none ->
                  case (btl_character:get_location(Char) == Location) of
                     false -> none;
                     true -> IX
                  end;

               _ -> CurrentResult
            end
         end,
         none,
         Characters
      ),

   case MaybeResultIX of
      none -> Update;
      _ -> perform_on_target(MaybeResultIX, Power, Update)
   end.


-spec standard_perform
   (
      btl_conditions:single(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
standard_perform (Condition, S0Update) ->
   Parameters = btl_conditions:get_parameters(Condition),
   Power =
      case btl_condition_parameters:get_other(Parameters) of
         N when (is_integer(N) and (N >= 0)) -> N;
         Other ->
            error({param, other, Other}),
            0
      end,

   Chance = btl_condition_parameters:get_chance(Parameters),
   Perform =
      case Chance of
         -1 -> true;
         _ -> (Chance =< shr_roll:percentage())
      end,

   case Perform of
      false -> S0Update;
      true ->
         S1Update =
            lists:foldl
            (
               fun (Location, CurrentUpdate) ->
                  perform_on_location(Location, Power, CurrentUpdate)
               end,
               S0Update,
               btl_condition_parameters:get_locations(Parameters)
            ),

         S2Update =
            lists:foldl
            (
               fun (TargetIX, CurrentUpdate) ->
                  perform_on_target(TargetIX, Power, CurrentUpdate)
               end,
               S1Update,
               btl_condition_parameters:get_targets(Parameters)
            ),

         S2Update
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply
   (
      btl_conditions:ref(),
      btl_character_turn_update:type(),
      shr_condition:context(any(), VolatileDataType)
   )
   -> {VolatileDataType, btl_character_turn_update:type()}.
apply (SelfRef, S0Update, S0Context) ->
   {_Trigger, _ReadOnlyData, VolatileData} = S0Context,

   case btl_conditions:get_condition(SelfRef, S0Update) of
      none -> {VolatileData, S0Update};
      {ok, S0Condition} ->
         % TODO: handle cases where the Volatile Data contains characters that
         % might have to be healed.
         S1Update = standard_perform(S0Condition, S0Update),

         {VolatileData, S1Update}
   end.

-spec encode_turn_result (any()) -> binary().
encode_turn_result ({CharIX, HealingAmount}) ->
   jiffy:encode
   (
      [
         {<<"ix">>, CharIX},
         {<<"p">>, HealingAmount}
      ]
   );
encode_turn_result (Other) ->
   error({turn_result, Other}).
