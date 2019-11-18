-module(btl_status_indicators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type visibility() ::
   (
      none
      | {limited, ordsets:ordset(non_neg_integer())} % PlayerIXs
      | all
   ).

-type ref() ::
   (
      {char, non_neg_integer(), non_neg_integer()}
      | {battle, non_neg_integer()}
   ).

-record
(
   btl_sti,
   {
      icon :: binary(),
      extra :: binary(),
      visibility :: visibility()
   }
).

-type single() :: #btl_cond{}.
-type type () :: orddict:orddict(non_neg_integer(), single()),

-export_type
(
   [
      type/0,
      ref/0,
      visibility/0,
      single/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_status_indicator/2,

      get_parameters/1,
      set_parameters/3, % IX, Value, Conditions
      ataxia_set_parameters/3, % IX, Value, Conditions
      ataxia_set_parameters/4, % IX, Value, Conditions

      get_triggers/1,
      set_triggers/3, % IX, Value, Conditions
      ataxia_set_triggers/3, % IX, Value, Conditions

      get_visibility/1,
      set_visibility/3, % IX, Value, Conditions
      ataxia_set_visibility/3 % IX, Value, Conditions
   ]
).

-export
(
   [
      add/5,
      ataxia_add/5,
      remove/2,
      ataxia_remove/2,
      new/0
   ]
).

-export
(
   [
      apply_to_character/5,
      apply_to_battle/4
   ]
).

-export
(
   [
      encode_for/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_trigger
   (
      shr_condition:context(_ReadOnlyDataType, VolatileDataType),
      fun((non_neg_integer()) -> ref()),
      btl_character_turn_update:type(),
      ordsets:ordset(non_neg_integer()),
      type()
   )
   -> {VolatileDataType, btl_character_turn_update:type()}.
apply_trigger (Context, IXtoRef, S0Update, RelevantIndices, Conditions) ->
   {Trigger, ReadOnlyData, S0VolatileData} = Context,

   ConditionCollection = Conditions#btl_conds.collection,

   {LastVolatileData, LastUpdate} =
      ordsets:fold
      (
         fun (IX, {CurrentVolatileData, CurrentUpdate}) ->
            case orddict:find(IX, ConditionCollection) of
               {ok, Condition} ->
                  Module =
                     shr_condition:get_module
                     (
                        shr_condition:from_id(Condition#btl_cond.category)
                     ),

                  {NextVolatileData, NextUpdate} =
                     erlang:apply
                     (
                        Module,
                        apply,
                        [
                           IXtoRef(IX),
                           CurrentUpdate,
                           {Trigger, ReadOnlyData, CurrentVolatileData}
                        ]
                     ),

                  {NextVolatileData, NextUpdate};

               error ->
                  % TODO: Remove the condition.
                  {CurrentVolatileData, CurrentUpdate}
            end
         end,
         {S0VolatileData, S0Update, []},
         RelevantIndices
      ),

   {LastVolatileData, LastUpdate}.

-spec compute_next_index (type()) -> non_neg_integer().
compute_next_index (Conditions) ->
   Collection = Conditions#btl_conds.collection,
   CollectionSize = orddict:size(Collection),
   Candidates = lists:seq(0, CollectionSize),

   Result =
      lists:foldr
      (
         fun (Candidate, CurrentResult) ->
            case is_integer(CurrentResult) of
               true -> CurrentResult;
               false ->
                  case orddict:is_key(Candidate, Collection) of
                     true -> none;
                     false -> Candidate
                  end
            end
         end,
         none,
         Candidates
      ),

   Result.

-spec encode_single (non_neg_integer(), single()) -> {list({binary(), any()})}.
encode_single (IX, Condition) ->
   Module =
      shr_condition:get_module
      (
         shr_condition:from_id(Condition#btl_cond.category)
      ),

   EncodedParameters =
      erlang:apply
      (
         Module,
         encode_parameters,
         [
            Condition#btl_cond.parameters
         ]
      ),

   {
      [
         {<<"ix">>, IX},
         {<<"p">>, EncodedParameters}
      ]
   }.

-spec get_relevant_condition_indices
   (
      shr_condition:trigger(),
      type()
   )
   -> ordsets:ordset(non_neg_integer()).
get_relevant_condition_indices(Trigger, Conditions) ->
   case orddict:find(Trigger, Conditions#btl_conds.from_trigger) of
      {ok, Result} -> Result;
      _ -> ordsets:new()
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Accessors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_condition
   (
      ref(),
      btl_character_turn_update:type()
   )
   -> ({ok, single()} | none).
get_condition ({battle, IX}, Update) ->
   Conditions =
      btl_battle:get_conditions(btl_character_turn_update:get_battle(Update)),

   case orddict:find(IX, Conditions#btl_conds.collection) of
      error -> none;
      Other -> Other
   end;
get_condition ({char, CharIX, CondIX}, Update) ->
   Conditions =
      btl_character:get_conditions
      (
         btl_battle:get_character
         (
            CharIX,
            btl_character_turn_update:get_battle(Update)
         )
      ),

   case orddict:find(CondIX, Conditions#btl_conds.collection) of
      error -> none;
      Other -> Other
   end.

%%%%%%%%%%%%%%%%%%%%
%%%% Visibility %%%%
%%%%%%%%%%%%%%%%%%%%
-spec get_visibility (single()) -> visibility().
get_visibility (Condition) -> Condition#btl_cond.visibility.

-spec set_visibility (non_neg_integer(), visibility(), type()) -> type().
set_visibility (IX, NewVisibility, Conditions) ->
   Conditions#btl_conds
   {
      collection =
         orddict:update
         (
            IX,
            fun (Condition) ->
               Condition#btl_cond{ visibility = NewVisibility }
            end,
            Conditions#btl_conds.collection
         )
   }.

-spec ataxia_set_visibility
   (
      non_neg_integer(),
      visibility(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_visibility (IX, NewVisibility, Conditions) ->
   {
      set_visibility(IX, NewVisibility, Conditions),
      ataxic:update_field
      (
         #btl_conds.collection,
         ataxic_sugar:update_orddict_element
         (
            IX,
            ataxic:update_field
            (
               #btl_cond.visibility,
               ataxic:constant(NewVisibility)
            )
         )
      )
   }.

%%%%%%%%%%%%%%%%%%
%%%% Triggers %%%%
%%%%%%%%%%%%%%%%%%
-spec get_triggers (single()) -> ordsets:ordset(shr_condition:trigger()).
get_triggers (Condition) -> Condition#btl_cond.triggers.

-spec set_triggers
   (
      non_neg_integer(),
      ordsets:ordset(shr_condition:trigger()),
      type()
   )
   -> type().
set_triggers (IX, NewTriggers, Conditions) ->
   CurrentCondition = orddict:fetch(IX, Conditions#btl_conds.collection),
   CurrentTriggers = CurrentCondition#btl_cond.triggers,
   AddedTriggers = ordsets:subtract(NewTriggers, CurrentTriggers),
   RemovedTriggers = ordsets:subtract(CurrentTriggers, NewTriggers),

   S0FromTrigger =
      ordsets:fold
      (
         fun (Trigger, FromTrigger) ->
            orddict:update
            (
               Trigger,
               fun (ConditionIXs) ->
                  ordsets:del_element(IX, ConditionIXs)
               end,
               FromTrigger
            )
         end,
         Conditions#btl_conds.from_trigger,
         RemovedTriggers
      ),

   S1FromTrigger =
      ordsets:fold
      (
         fun (Trigger, FromTrigger) ->
            orddict:update
            (
               Trigger,
               fun (ConditionIXs) ->
                  ordsets:add_element(IX, ConditionIXs)
               end,
               FromTrigger
            )
         end,
         S0FromTrigger,
         AddedTriggers
      ),

   Conditions#btl_conds
   {
      from_trigger = S1FromTrigger,
      collection =
         orddict:update
         (
            IX,
            fun (Condition) -> Condition#btl_cond{ triggers = NewTriggers } end,
            Conditions#btl_conds.collection
         )
   }.

-spec ataxia_set_triggers
   (
      non_neg_integer(),
      ordsets:ordset(shr_condition:trigger()),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_triggers (IX, NewTriggers, Conditions) ->
   CurrentCondition = orddict:fetch(IX, Conditions#btl_conds.collection),
   CurrentTriggers = CurrentCondition#btl_cond.triggers,
   AddedTriggers = ordsets:subtract(NewTriggers, CurrentTriggers),
   RemovedTriggers = ordsets:subtract(CurrentTriggers, NewTriggers),
   AtaxicFromTriggerParams = [ataxic:constant(IX), ataxic:current_value()],

   {S0FromTrigger, S0FromTriggerAtaxicUpdates} =
      ordsets:fold
      (
         fun (Trigger, {FromTrigger, PrevFromTriggerAtaxicUpdates}) ->
            {
               orddict:update
               (
                  Trigger,
                  fun (ConditionIXs) ->
                     ordsets:del_element(IX, ConditionIXs)
                  end,
                  FromTrigger
               ),
               [
                  ataxic_sugar:update_orddict_element
                  (
                     IX,
                     ataxic:apply_function
                     (
                        ordsets,
                        del_element,
                        AtaxicFromTriggerParams
                     )
                  )
                  | PrevFromTriggerAtaxicUpdates
               ]
            }
         end,
         {Conditions#btl_conds.from_trigger, []},
         RemovedTriggers
      ),

   {S1FromTrigger, S1FromTriggerAtaxicUpdates} =
      ordsets:fold
      (
         fun (Trigger, {FromTrigger, PrevFromTriggerAtaxicUpdates}) ->
            {
               orddict:update
               (
                  Trigger,
                  fun (ConditionIXs) ->
                     ordsets:add_element(IX, ConditionIXs)
                  end,
                  FromTrigger
               ),
               [
                  ataxic_sugar:update_orddict_element
                  (
                     IX,
                     ataxic:apply_function
                     (
                        ordsets,
                        add_element,
                        AtaxicFromTriggerParams
                     )
                  )
                  | PrevFromTriggerAtaxicUpdates
               ]
            }
         end,
         {S0FromTrigger, S0FromTriggerAtaxicUpdates},
         AddedTriggers
      ),

   {
      Conditions#btl_conds
      {
         from_trigger = S1FromTrigger,
         collection =
            orddict:update
            (
               IX,
               fun (Condition) ->
                  Condition#btl_cond{ triggers = NewTriggers }
               end,
               Conditions#btl_conds.collection
            )
      },
      ataxic:sequence
      (
         [
            ataxic:update_field
            (
               #btl_conds.collection,
               ataxic_sugar:update_orddict_element
               (
                  IX,
                  ataxic:update_field
                  (
                     #btl_cond.triggers,
                     ataxic:sequence
                     (
                        [
                           ataxic:apply_function
                           (
                              ordsets,
                              subtract,
                              [
                                 ataxic:current_value(),
                                 ataxic:constant(RemovedTriggers)
                              ]
                           ),
                           ataxic:apply_function
                           (
                              ordsets,
                              union,
                              [
                                 ataxic:current_value(),
                                 ataxic:constant(AddedTriggers)
                              ]
                           )
                        ]
                     )
                  )
               )
            ),
            ataxic:update_field
            (
               #btl_conds.from_trigger,
               ataxic:sequence(S1FromTriggerAtaxicUpdates)
            )
         ]
      )
   }.

%%%%%%%%%%%%%%%%%%%%
%%%% Parameters %%%%
%%%%%%%%%%%%%%%%%%%%
-spec get_parameters (single()) -> any().
get_parameters (Condition) -> Condition#btl_cond.parameters.

-spec set_parameters (non_neg_integer(), any(), type()) -> type().
set_parameters (IX, NewValue, Conditions) ->
   Conditions#btl_conds
   {
      collection =
         orddict:update
         (
            IX,
            fun (Condition) -> Condition#btl_cond{ parameters = NewValue } end,
            Conditions#btl_conds.collection
         )
   }.

-spec ataxia_set_parameters
   (
      non_neg_integer(),
      any(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_parameters (IX, NewValue, Conditions) ->
   ataxia_set_parameters(IX, NewValue, ataxic:constant(NewValue), Conditions).

-spec ataxia_set_parameters
   (
      non_neg_integer(),
      any(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_parameters (IX, NewValue, ParamsAtaxicUpdate, Conditions) ->
   {
      set_parameters(IX, NewValue, Conditions),
      ataxic:update_field
      (
         #btl_conds.collection,
         ataxic_sugar:update_orddict_element
         (
            IX,
            ataxic:update_field
            (
               #btl_cond.parameters,
               ParamsAtaxicUpdate
            )
         )
      )
   }.

%%%% Apply %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_to_character
   (
      non_neg_integer(),
      shr_condition:trigger(),
      any(),
      VolatileDataType,
      btl_character_turn_update:type()
   )
   -> {VolatileDataType, btl_character_turn_update:type()}.
apply_to_character
(
   ActorIX,
   Trigger,
   ReadOnlyData,
   S0VolatileData,
   S0Update
) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),
   {S0Actor, S1Battle} = btl_battle:get_resolved_character(ActorIX, S0Battle),
   S1Update = btl_character_turn_update:set_battle(S1Battle, S0Update),

   CharacterConditions = btl_character:get_conditions(S0Actor),
   MatchingConditionIndices =
      get_relevant_condition_indices(Trigger, CharacterConditions),

   case ordsets:is_empty(MatchingConditionIndices) of
      true -> {S0VolatileData, S1Update};
      false ->
         {S1VolatileContext, S2Update} =
            apply_trigger
            (
               {Trigger, ReadOnlyData, S0VolatileData},
               fun (IX) -> {char, ActorIX, IX} end,
               S1Update,
               MatchingConditionIndices,
               CharacterConditions
            ),

         {S1VolatileContext, S2Update}
   end.

-spec apply_to_battle
   (
      shr_condition:trigger(),
      any(),
      VolatileDataType,
      btl_character_turn_update:type()
   )
   -> {VolatileDataType, btl_character_turn_update:type()}.
apply_to_battle
(
   Trigger,
   ReadOnlyData,
   S0VolatileData,
   S0Update
) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),

   BattleConditions = btl_battle:get_conditions(S0Battle),
   MatchingConditionIndices =
      get_relevant_condition_indices(Trigger, BattleConditions),

   case ordsets:is_empty(MatchingConditionIndices) of
      true -> {S0VolatileData, S0Update};
      false ->
         {S1VolatileContext, S1Update} =
            apply_trigger
            (
               {Trigger, ReadOnlyData, S0VolatileData},
               fun (IX) -> {battle, IX} end,
               S0Update,
               MatchingConditionIndices,
               BattleConditions
            ),

         {S1VolatileContext, S1Update}
   end.

%%%% Add/Remove Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%
%%%% Add %%%%
%%%%%%%%%%%%%
-spec add
   (
      shr_condition:id(),
      ordsets:ordset(shr_condition:trigger()),
      any(),
      visibility(),
      type()
   )
   -> {type(), non_neg_integer()}.
add (CondID, Triggers, Params, Visibility, Conditions) ->
   NewCondition =
      #btl_cond
      {
         category = CondID,
         triggers = Triggers,
         parameters = Params,
         visibility = Visibility
      },

   NewConditionIX = compute_next_index(Conditions),

   UpdatedCollection =
      orddict:store
      (
         NewConditionIX,
         NewCondition,
         Conditions#btl_conds.collection
      ),

   UpdatedFromTrigger =
      ordsets:fold
      (
         fun (Trigger, FromTrigger) ->
            orddict:update
            (
               Trigger,
               fun (Set) -> ordsets:add_element(NewConditionIX, Set) end,
               FromTrigger
            )
         end,
         Conditions#btl_conds.from_trigger,
         Triggers
      ),

   {
      Conditions#btl_conds
      {
         collection = UpdatedCollection,
         from_trigger = UpdatedFromTrigger
      },
      NewConditionIX
   }.

-spec ataxia_add
   (
      shr_condition:id(),
      ordsets:ordset(shr_condition:trigger()),
      any(),
      visibility(),
      type()
   )
   -> {type(), non_neg_integer(), ataxic:basic()}.
ataxia_add (CondID, Triggers, Params, Visibility, Conditions) ->
   NewCondition =
      #btl_cond
      {
         category = CondID,
         triggers = Triggers,
         parameters = Params,
         visibility = Visibility
      },

   NewConditionIX = compute_next_index(Conditions),
   AtaxicNewConditionIX = ataxic:constant(NewConditionIX),

   UpdatedCollection =
      orddict:store
      (
         NewConditionIX,
         NewCondition,
         Conditions#btl_conds.collection
      ),

   CollectionAtaxicUpdate =
      ataxic:apply_function
      (
         orddict,
         store,
         [
            AtaxicNewConditionIX,
            ataxic:constant(NewCondition),
            ataxic:current_value()
         ]
      ),

   SetAtaxicUpdate =
      ataxic:apply_function
      (
         ordsets,
         add_element,
         [
            AtaxicNewConditionIX,
            ataxic:current_value()
         ]
      ),

   {UpdatedFromTrigger, FromTriggerAtaxicUpdateList} =
      ordsets:fold
      (
         fun (Trigger, {FromTrigger, FromTriggerUpdates}) ->
            {
               orddict:update
               (
                  Trigger,
                  fun (Set) -> ordsets:add_element(NewConditionIX, Set) end,
                  FromTrigger
               ),
               [
                  ataxic_sugar:update_orddict_element
                  (
                     Trigger,
                     SetAtaxicUpdate
                  )
                  | FromTriggerUpdates
               ]
            }
         end,
         Conditions#btl_conds.from_trigger,
         Triggers
      ),

   {
      Conditions#btl_conds
      {
         collection = UpdatedCollection,
         from_trigger = UpdatedFromTrigger
      },
      NewConditionIX,
      ataxic:sequence
      (
         [
            ataxic:update_field
            (
               #btl_conds.collection,
               CollectionAtaxicUpdate
            ),
            ataxic:update_field
            (
               #btl_conds.from_trigger,
               ataxic:sequence(FromTriggerAtaxicUpdateList)
            )
         ]
      )
   }.

%%%%%%%%%%%%%%%%
%%%% Remove %%%%
%%%%%%%%%%%%%%%%
-spec remove (non_neg_integer(), type()) -> type().
remove (IX, S0Conditions) ->
   S1Conditions = set_triggers(IX, ordsets:new(), S0Conditions),
   S2Conditions =
      S1Conditions#btl_conds
      {
         collection = orddict:erase(IX, S1Conditions#btl_conds.collection)
      },

   S2Conditions.

-spec ataxia_remove (non_neg_integer(), type()) -> {type(), ataxic:basic()}.
ataxia_remove (IX, S0Conditions) ->
   {S1Conditions, ConditionsAtaxicUpdate1} =
      ataxia_set_triggers(IX, ordsets:new(), S0Conditions),

   S2Conditions =
      S1Conditions#btl_conds
      {
         collection = orddict:erase(IX, S1Conditions#btl_conds.collection)
      },

   {
      S2Conditions,
      ataxic:sequence
      (
         [
            ConditionsAtaxicUpdate1,
            ataxic:update_field
            (
               #btl_conds.collection,
               ataxic:apply_function
               (
                  orddict,
                  erase,
                  [
                     ataxic:constant(IX),
                     ataxic:current_value()
                  ]
               )
            )
         ]
      )
   }.

%%%% Other %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new () -> type().
new () ->
   #btl_conds
   {
      collection = orddict:new(),
      from_trigger = orddict:new()
   }.

-spec encode_for (non_neg_integer(), type()) -> list(any()).
encode_for (PlayerIX, Conditions) ->
   lists:filtermap
   (
      fun ({IX, Condition}) ->
         case Condition#btl_cond.visibility of
            none -> false;
            all -> encode_single(IX, Condition);
            {limited, AllowedPlayerIXs} ->
               case ordsets:is_element(PlayerIX, AllowedPlayerIXs) of
                  false -> false;
                  true -> {true, encode_single(IX, Condition)}
               end
         end
      end,
      orddict:to_list(Conditions#btl_conds.collection)
   ).