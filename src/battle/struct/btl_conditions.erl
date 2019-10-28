-module(btl_conditions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("tacticians/conditions.hrl").

-type visibility() ::
   (
      none
      | {limited, ordsets:ordsets(non_neg_integer())} % PlayerIXs
      | all
   ).

-type update() ::
   (
      none
      | remove
      | {update, ataxic:basic()}
   ).

-type ref() ::
   (
      {char, non_neg_integer(), non_neg_integer()}
      | {battle, non_neg_integer()}
   ).

-record
(
   btl_cond,
   {
      category :: shr_condition:id(),
      triggers :: ordsets:ordset(shr_condition:trigger()),
      parameters :: tuple(),
      visibility :: visibility()
   }
).

-opaque single() :: #btl_cond{}.

-record
(
   btl_conds,
   {
      collection :: orddict:orddict(non_neg_integer(), single()),
      from_trigger ::
         orddict:orddict
         (
            shr_condition:trigger(),
            ordset:ordset(non_neg_integer())
         )
   }
).

-opaque type() :: #btl_conds{}.

-export_type([type/0, ref/0, visibility/0, update/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_parameters_field/0,

      add/5,
      ataxia_add/5,
      new/0
   ]
).

-export
(
   [
      apply_to_character/5,
      apply_to_battle/4,
      update_from_reference/3
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
-spec encode_parameters
   (
      shr_condition:id(),
      tuple()
   )
   -> {list({binary(), any()})}.
encode_parameters (_Category, _Parameters) -> {[]}. % TODO.

-spec updates_to_ataxic_update
   (
      list({non_neg_integer(), update()})
   )
   -> ataxic:basic().
updates_to_ataxic_update (Updates) ->
   AtaxicSequence =
      lists:foldl
      (
         fun ({IX, Update}, AtaxicUpdates) ->
            case Update of
               none -> AtaxicUpdates;
               remove ->
                  [
                     ataxic:apply_function
                     (
                        orddict,
                        erase,
                        [ataxic:constant(IX), ataxic:current_value()]
                     )
                     |AtaxicUpdates
                  ];

               {update, Ataxic} ->
                  [
                     ataxic_sugar:update_orddict_element(IX, Ataxic)
                     |AtaxicUpdates
                  ]
            end
         end,
         [],
         Updates
      ),

   ataxic:sequence(AtaxicSequence).

-spec ataxia_apply_trigger
   (
      shr_condition:context(_ReadOnlyDataType, VolatileDataType),
      btl_character_turn_update:type(),
      collection()
   )
   ->
   {
      VolatileDataType,
      ataxic:basic(),
      btl_character_turn_update:type()
   }.
ataxia_apply_trigger (Context, S0Update, Conditions) ->
   {Trigger, ReadOnlyData, S0VolatileData} = Context,

   RelevantConditions =
      orddict:filter
      (
         fun (_IX, Condition) ->
            ordsets:is_element(Trigger, Condition#btl_cond.triggers)
         end,
         Conditions
      ),

   {LastVolatileData, LastUpdate, AllUpdateActions} =
      orddict:fold
      (
         fun
         (
            IX,
            Condition,
            {
               CurrentVolatileData,
               CurrentUpdate,
               UpdateActions
            }
         ) ->
            Module =
               shr_condition:get_module
               (
                  shr_condition:from_id(Condition#btl_cond.category)
               ),

            {NextVolatileData, NextUpdate, UpdateAction} =
               erlang:apply
               (
                  Module,
                  apply,
                  [
                     Condition,
                     CurrentUpdate,
                     {Trigger, ReadOnlyData, CurrentVolatileData}
                  ]
               ),

            {
               NextVolatileData,
               NextUpdate,
               case UpdateAction of
                  none -> UpdateActions;
                  _ -> [{IX, UpdateAction}|UpdateActions]
               end
            }
         end,
         {S0VolatileData, S0Update, []},
         RelevantConditions
      ),

   ConditionsAtaxiaUpdate = updates_to_ataxic_update(AllUpdateActions),

   {LastVolatileData, ConditionsAtaxiaUpdate, LastUpdate}.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

   {
      S1VolatileContext,
      ActorConditionsAtaxicUpdate,
      S2Update
   } =
      ataxia_apply_trigger
      (
         {Trigger, ReadOnlyData, S0VolatileData},
         S1Update,
         btl_character:get_conditions(S0Actor)
      ),

   %%%%% Actor and Battle may have been modified %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   S1Battle = btl_character_turn_update:get_battle(S2Update),
   {S1Actor, S2Battle} = btl_battle:get_resolved_character(ActorIX, S1Battle),
   S0Conditions = btl_character:get_conditions(S1Actor),

   S1Conditions =
      ataxic:apply_basic_to(ActorConditionsAtaxicUpdate, S0Conditions),

   {S2Actor, ActorAtaxicUpdate} =
      btl_character:ataxia_set_conditions
      (
         S1Conditions,
         ActorConditionsAtaxicUpdate,
         S1Actor
      ),

   {S3Battle, BattleAtaxicUpdate} =
      btl_battle:ataxia_set_character
      (
         ActorIX,
         S2Actor,
         ActorAtaxicUpdate,
         S2Battle
      ),

   S2Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S3Battle,
         BattleAtaxicUpdate,
         S1Update
      ),

   {S1VolatileContext, S2Update}.

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

   {
      S1VolatileContext,
      BattleConditionsAtaxicUpdate,
      S1Update
   } =
      ataxia_apply_trigger
      (
         {Trigger, ReadOnlyData, S0VolatileData},
         S0Update,
         btl_battle:get_conditions(S0Battle)
      ),

   %%%% Battle may have been modified (and very likely has) %%%%%%%%%%%%%%%%%%%%
   S1Battle = btl_character_turn_update:get_battle(S1Update),
   UpdatedBattleConditions =
      ataxic:apply_basic_to
      (
         BattleConditionsAtaxicUpdate,
         btl_battle:get_conditions(S1Battle)
      ),

   {S2Battle, BattleAtaxicUpdate} =
      btl_battle:ataxia_set_conditions(UpdatedBattleConditions, S1Battle),

   S1Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S2Battle,
         BattleAtaxicUpdate,
         S1Update
      ),

   {S1VolatileContext, S1Update}.

-spec update_from_reference
   (
      ref(),
      update(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
update_from_reference ({battle, _CondIX}, _UpdateAction, Update) ->
   Update; % TODO
update_from_reference ({char, _CharIX, _CondIX}, _UpdateAction, Update) ->
   Update. % TODO

-spec add
   (
      shr_condition:id(),
      ordsets:ordset(shr_condition:trigger()),
      tuple(),
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
      ordset:fold
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
      tuple(),
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
      ordset:fold
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
                  |FromTriggerUpdates
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

-spec new () -> type().
new () ->
   #btl_conds
   {
      collection = orddict:new(),
      from_trigger = orddict:new()
   }.

-spec encode_single
   (
      non_neg_integer(),
      type()
   )
   -> list({binary(), any()}).
encode_single (IX, Condition) -> todo. % TODO

-spec encode_for (non_neg_integer(), type()) -> list({binary(), any()}).
encode_for (PlayerIX, Conditions) ->
   lists:filtermap
   (
      fun ({IX, Condition}) ->
         case Condition#btl_cond.visibility of
            none -> false;
            any -> encode(IX, Condition);
            {limited, AllowedPlayerIXs} ->
               case ordsets:is_element(PlayerIX, AllowedPlayerIXs) of
                  false -> false;
                  true -> {true, encode(IX, Condition)}
               end
         end
      end,
      orddict:to_list(Conditions)
   ).

-spec get_parameters_field () -> non_neg_integer().
get_parameters_field () -> #btl_cond.parameters.

-spec get_visibility_field () -> non_neg_integer().
get_visibility_field () -> #btl_cond.visibility.
