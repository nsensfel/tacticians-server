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
      | {map, non_neg_integer()}
   ).

-record
(
   btl_sti,
   {
      category :: binary(),
      parameter :: binary(),
      visibility :: visibility()
   }
).

-type single() :: #btl_sti{}.
-type type() :: orddict:orddict(non_neg_integer(), single()).

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

      get_parameter/1,
      set_parameter/3, % IX, Value, StatusIndicators
      ataxia_set_parameter/3, % IX, Value, StatusIndicators

      get_category/1,
      set_category/3, % IX, Value, StatusIndicators
      ataxia_set_category/3, % IX, Value, StatusIndicators

      get_visibility/1,
      set_visibility/3, % IX, Value, StatusIndicators
      ataxia_set_visibility/3 % IX, Value, StatusIndicators
   ]
).

-export
(
   [
      add_at/5,
      ataxia_add/5,
      add/4,
      ataxia_add/4,
      remove/2,
      ataxia_remove/2,
      new/0
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
-spec encode_single (non_neg_integer(), single()) -> {list({binary(), any()})}.
encode_single (IX, StatusIndicator) ->
   Category = StatusIndicator#btl_sti.category,
   Parameter = StatusIndicator#btl_sti.parameter,

   {
      [
         % XXX: This exposes the presence of hidden status indicators
         {<<"i">>, IX},
         {<<"c">>, Category},
         {<<"p">>, Parameter}
      ]
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Accessors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_status_indicator
   (
      ref(),
      btl_character_turn_update:type()
   )
   -> ({ok, single()} | none).
get_status_indicator ({battle, IX}, Update) ->
   StatusIndicators =
      btl_battle:get_status_indicators
      (
         btl_character_turn_update:get_battle(Update)
      ),

   case orddict:find(IX, StatusIndicators) of
      error -> none;
      Other -> Other
   end;
get_status_indicator ({char, CharIX, StatusIndicatorIX}, Update) ->
   StatusIndicators =
      btl_character:get_status_indicators
      (
         btl_battle:get_character
         (
            CharIX,
            btl_character_turn_update:get_battle(Update)
         )
      ),

   case orddict:find(StatusIndicatorIX, StatusIndicators) of
      error -> none;
      Other -> Other
   end.

%%%%%%%%%%%%%%%%%%%%
%%%% Visibility %%%%
%%%%%%%%%%%%%%%%%%%%
-spec get_visibility (single()) -> visibility().
get_visibility (StatusIndicator) -> StatusIndicator#btl_sti.visibility.

-spec set_visibility (non_neg_integer(), visibility(), type()) -> type().
set_visibility (IX, NewVisibility, StatusIndicators) ->
   orddict:update
   (
      IX,
      fun (StatusIndicator) ->
         StatusIndicator#btl_sti{ visibility = NewVisibility }
      end,
      StatusIndicators
   ).

-spec ataxia_set_visibility
   (
      non_neg_integer(),
      visibility(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_visibility (IX, NewVisibility, StatusIndicators) ->
   {
      set_visibility(IX, NewVisibility, StatusIndicators),
      ataxic_sugar:update_orddict_element
      (
         IX,
         ataxic:update_field
         (
            #btl_sti.visibility,
            ataxic:constant(NewVisibility)
         )
      )
   }.

%%%%%%%%%%%%%%%%%%
%%%% Category %%%%
%%%%%%%%%%%%%%%%%%
-spec get_category (single()) -> binary().
get_category (StatusIndicator) -> StatusIndicator#btl_sti.category.

-spec set_category
   (
      non_neg_integer(),
      binary(),
      type()
   )
   -> type().
set_category (IX, NewValue, StatusIndicators) ->
   orddict:update
   (
      IX,
      fun (StatusIndicator) ->
         StatusIndicator#btl_sti{ category = NewValue }
      end,
      StatusIndicators
   ).

-spec ataxia_set_category
   (
      non_neg_integer(),
      binary(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_category (IX, NewValue, StatusIndicators) ->
   {
      set_category(IX, NewValue, StatusIndicators),
      ataxic_sugar:update_orddict_element
      (
         IX,
         ataxic:update_field
         (
            #btl_sti.category,
            ataxic:constant(NewValue)
         )
      )
   }.


%%%%%%%%%%%%%%%%%%%%
%%%% Parameters %%%%
%%%%%%%%%%%%%%%%%%%%
-spec get_parameter (single()) -> binary().
get_parameter (StatusIndicator) -> StatusIndicator#btl_sti.parameter.

-spec set_parameter (non_neg_integer(), binary(), type()) -> type().
set_parameter (IX, NewValue, StatusIndicators) ->
   orddict:update
   (
      IX,
      fun (StatusIndicator) ->
         StatusIndicator#btl_sti{ parameter = NewValue }
      end,
      StatusIndicators
   ).

-spec ataxia_set_parameter
   (
      non_neg_integer(),
      binary(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_parameter (IX, NewValue, StatusIndicators) ->
   {
      set_parameter(IX, NewValue, StatusIndicators),
      ataxic_sugar:update_orddict_element
      (
         IX,
         ataxic:update_field
         (
            #btl_sti.parameter,
            ataxic:constant(NewValue)
         )
      )
   }.

%%%% Add/Remove Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%
%%%% Add %%%%
%%%%%%%%%%%%%
-spec add
   (
      binary(),
      binary(),
      visibility(),
      type()
   )
   -> {type(), non_neg_integer()}.
add (Category, Parameter, Visibility, S0StatusIndicators) ->
   NewStatusIndicator =
      #btl_sti
      {
         category = Category,
         parameter = Parameter,
         visibility = Visibility
      },

   NewStatusIndicatorIX =
      shr_orddict_util:compute_next_non_neg_integer_index(S0StatusIndicators),

   S1StatusIndicators =
      orddict:store
      (
         NewStatusIndicatorIX,
         NewStatusIndicator,
         S0StatusIndicators
      ),

   {S1StatusIndicators, NewStatusIndicatorIX}.

-spec ataxia_add
   (
      binary(),
      binary(),
      visibility(),
      type()
   )
   -> {type(), non_neg_integer(), ataxic:basic()}.
ataxia_add (Category, Parameter, Visibility, S0StatusIndicators) ->
   NewStatusIndicator =
      #btl_sti
      {
         category = Category,
         parameter = Parameter,
         visibility = Visibility
      },

   NewStatusIndicatorIX =
      shr_orddict_util:compute_next_non_neg_integer_index(S0StatusIndicators),

   S1StatusIndicators =
      orddict:store
      (
         NewStatusIndicatorIX,
         NewStatusIndicator,
         S0StatusIndicators
      ),


   StatusIndicatorsAtaxicUpdate =
      ataxic:apply_function
      (
         orddict,
         store,
         [
            ataxic:constant(NewStatusIndicatorIX),
            ataxic:constant(NewStatusIndicator),
            ataxic:current_value()
         ]
      ),


   {
      S1StatusIndicators,
      NewStatusIndicatorIX,
      StatusIndicatorsAtaxicUpdate
   }.

%%%%%%%%%%%%%%%%
%%%% Remove %%%%
%%%%%%%%%%%%%%%%
-spec remove (non_neg_integer(), type()) -> type().
remove (IX, S0StatusIndicators) ->
   S1StatusIndicators = orddict:erase(IX, S0StatusIndicators),

   S1StatusIndicators.

-spec ataxia_remove (non_neg_integer(), type()) -> {type(), ataxic:basic()}.
ataxia_remove (IX, S0StatusIndicators) ->
   S1StatusIndicators = orddict:erase(IX, S0StatusIndicators),

   {
      S1StatusIndicators,
      ataxic:apply_function
      (
         orddict,
         erase,
         [
            ataxic:constant(IX),
            ataxic:current_value()
         ]
      )
   }.

%%%% Other %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new () -> type().
new () ->
   orddict:new().

-spec encode_for (non_neg_integer(), type()) -> list(any()).
encode_for (PlayerIX, StatusIndicators) ->
   lists:filtermap
   (
      fun ({IX, StatusIndicator}) ->
         case StatusIndicator#btl_sti.visibility of
            none -> false;
            all -> encode_single(IX, StatusIndicator);
            {limited, AllowedPlayerIXs} ->
               case ordsets:is_element(PlayerIX, AllowedPlayerIXs) of
                  false -> false;
                  true -> {true, encode_single(IX, StatusIndicator)}
               end
         end
      end,
      orddict:to_list(StatusIndicators)
   ).
