-module(btl_condition).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   btl_cond,
   {
      category :: shr_condition:id(),
      triggers :: ordset:ordset(shr_condition:trigger()),
      occurrences :: (non_neg_integer() | -1),
      duration :: (non_neg_integer() | -1),
      parameters :: tuple()
   }
).

-opaque type() :: #btl_cond{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_category/1,
      get_triggers/1,
      get_remaining_occurrences/1,
      get_duration/1,
      get_parameters/1,

      set_triggers/2,
      set_remaining_occurrences/2,
      set_duration/2,
      set_parameters/2,

      ataxia_set_triggers/2,
      ataxia_set_remaining_occurrences/2,
      ataxia_set_duration/2,
      ataxia_set_parameters/2,

      ataxia_set_triggers/3,
      ataxia_set_parameters/3,

      get_category_field/0,
      get_triggers_field/0,
      get_remaining_occurrences_field/0,
      get_duration_field/0,
      get_parameters_field/0
   ]
).

-export
(
   [
      triggers_on/2
   ]
).

-export
(
   [
      encode/1
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_category (type()) -> shr_condition:id().
get_category (Condition) -> Condition#btl_cond.category.

-spec get_triggers (type()) -> ordset:ordset(shr_condition:trigger()).
get_triggers (Condition) -> Condition#btl_cond.triggers.

-spec get_remaining_occurrences (type()) -> (non_neg_integer() | -1).
get_remaining_occurrences (Condition) -> Condition#btl_cond.occurrences.

-spec get_duration (type()) -> (non_neg_integer() | -1).
get_duration (Condition) -> Condition#btl_cond.duration.

-spec get_parameters (type()) -> tuple().
get_parameters (Condition) -> Condition#btl_cond.parameters.

-spec set_triggers (ordset:ordset(shr_condition:trigger()), type()) -> type().
set_triggers (Triggers, Condition) -> Condition#btl_cond{ triggers = Triggers }.

-spec set_remaining_occurrences ((non_neg_integer() | -1), type()) -> type().
set_remaining_occurrences (Value, Condition) ->
   Condition#btl_cond{ occurrences = Value }.

-spec set_duration ((non_neg_integer() | -1), type()) -> type().
set_duration (Value, Condition) ->
   Condition#btl_cond{ duration = Value }.

-spec set_parameters (tuple(), type()) -> type().
set_parameters (Value, Condition) -> Condition#btl_cond{ parameters = Value }.

-spec ataxia_set_triggers
   (
      ordset:ordset(shr_condition:trigger()),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_triggers (Triggers, Condition) ->
   {
      set_triggers(Triggers, Condition),
      ataxic:update_field
      (
         get_triggers_field(),
         ataxic:constant(Triggers)
      )
   }.

-spec ataxia_set_remaining_occurrences
   (
      (non_neg_integer() | -1),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_remaining_occurrences (Value, Condition) ->
   {
      set_remaining_occurrences(Value, Condition),
      ataxic:update_field
      (
         get_remaining_occurrences_field(),
         ataxic:constant(Value)
      )
   }.


-spec ataxia_set_duration
   (
      (non_neg_integer() | -1),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_duration (Value, Condition) ->
   {
      set_duration(Value, Condition),
      ataxic:update_field
      (
         get_duration_field(),
         ataxic:constant(Value)
      )
   }.

-spec ataxia_set_parameters (tuple(), type()) -> {type(), ataxic:basic()}.
ataxia_set_parameters (Value, Condition) ->
   {
      set_parameters(Value, Condition),
      ataxic:update_field
      (
         get_parameters_field(),
         ataxic:constant(Value)
      )
   }.

-spec ataxia_set_triggers
   (
      ordset:ordset(shr_condition:trigger()),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_triggers (Triggers, Update, Condition) ->
   {
      set_triggers(Triggers, Condition),
      ataxic:update_field
      (
         get_triggers_field(),
         Update
      )
   }.

-spec ataxia_set_parameters
   (
      tuple(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_parameters (Value, Update, Condition) ->
   {
      set_parameters(Value, Condition),
      ataxic:update_field
      (
         get_parameters_field(),
         Update
      )
   }.

-spec triggers_on (shr_condition:trigger(), type()) -> boolean().
triggers_on (Trigger, Type) ->
   ordset:is_element(Trigger, Type#btl_cond.triggers).

-spec get_category_field () -> non_neg_integer().
get_category_field () -> #btl_cond.category.

-spec get_triggers_field () -> non_neg_integer().
get_triggers_field () -> #btl_cond.triggers.

-spec get_remaining_occurrences_field () -> non_neg_integer().
get_remaining_occurrences_field () -> #btl_cond.occurrences.

-spec get_duration_field () -> non_neg_integer().
get_duration_field () -> #btl_cond.duration.

-spec get_parameters_field () -> non_neg_integer().
get_parameters_field () -> #btl_cond.parameters.

-spec encode (type()) -> {list({binary(), any()})}.
encode (Condition) -> {[]} % TODO.
