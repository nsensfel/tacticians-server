-module(btl_condition_parameters).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   btl_cond_params,
   {
      targets :: list(non_neg_integer()),
      locations :: list(shr_location:type()),
      uses :: (non_neg_integer() | -1),
      duration :: (non_neg_integer() | -1),
      chance :: (0..100 | -1),
      other :: any()
   }
).

-type type(OtherDataType) :: #btl_cond_params{ other :: OtherDataType }.

-export_type
(
   [
      type/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_targets/1,
      set_targets/2,
      ataxia_set_targets/2,
      ataxia_set_targets/3,

      get_uses/1,
      set_uses/2,
      ataxia_set_uses/2,

      get_duration/1,
      set_duration/2,
      ataxia_set_duration/2,

      get_chance/1,
      set_chance/2,
      ataxia_set_chance/2,

      get_other/1,
      set_other/2,
      ataxia_set_other/2,
      ataxia_set_other/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%
%%%% Targets %%%%
%%%%%%%%%%%%%%%%%
-spec get_targets (type(_)) -> list(non_neg_integer()).
get_targets (Params) -> Params#btl_cond_params.targets.

-spec set_targets (list(non_neg_integer()), type(ODT)) -> type(ODT).
set_targets (Targets, Params) -> Params#btl_cond_params{ targets = Targets }.

-spec ataxia_set_targets
   (
      list(non_neg_integer()),
      type(ODT)
   )
   -> {type(ODT), ataxic:basic()}.
ataxia_set_targets (Targets, Params) ->
   ataxia_set_targets(Targets, ataxic:constant(Targets), Params).

-spec ataxia_set_targets
   (
      list(non_neg_integer()),
      ataxic:basic(),
      type(ODT)
   )
   -> {type(ODT), ataxic:basic()}.
ataxia_set_targets (Targets, TargetsAtaxicUpdate, Params) ->
   {
      set_targets(Targets, Params),
      ataxic:update_field(#btl_cond_params.targets, TargetsAtaxicUpdate)
   }.

%%%%%%%%%%%%%%
%%%% Uses %%%%
%%%%%%%%%%%%%%
-spec get_uses (type(_)) -> (non_neg_integer() | -1).
get_uses (Params) -> Params#btl_cond_params.uses.

-spec set_uses ((non_neg_integer() | -1), type(ODT)) -> type(ODT).
set_uses (Uses, Params) -> Params#btl_cond_params{ uses = Uses }.

-spec ataxia_set_uses
   (
      (non_neg_integer() | -1),
      type(ODT)
   )
   -> {type(ODT), ataxic:basic()}.
ataxia_set_uses (Uses, Params) ->
   {
      set_uses(Uses, Params),
      ataxic:update_field(#btl_cond_params.uses, ataxic:constant(Uses))
   }.

%%%%%%%%%%%%%%%%%%
%%%% Duration %%%%
%%%%%%%%%%%%%%%%%%
-spec get_duration (type(_)) -> (non_neg_integer() | -1).
get_duration (Params) -> Params#btl_cond_params.duration.

-spec set_duration ((non_neg_integer() | -1), type(ODT)) -> type(ODT).
set_duration (Duration, Params) ->
   Params#btl_cond_params{ duration = Duration }.

-spec ataxia_set_duration
   (
      (non_neg_integer() | -1),
      type(ODT)
   )
   -> {type(ODT), ataxic:basic()}.
ataxia_set_duration (Duration, Params) ->
   {
      set_duration(Duration, Params),
      ataxic:update_field(#btl_cond_params.duration, ataxic:constant(Duration))
   }.

%%%%%%%%%%%%%%%%
%%%% Chance %%%%
%%%%%%%%%%%%%%%%
-spec get_chance (type(_)) -> (0..100 | -1).
get_chance (Params) -> Params#btl_cond_params.chance.

-spec set_chance ((0..100 | -1), type(ODT)) -> type(ODT).
set_chance (Chance, Params) -> Params#btl_cond_params{ chance = Chance }.

-spec ataxia_set_chance
   (
      (0..100 | -1),
      type(ODT)
   )
   -> {type(ODT), ataxic:basic()}.
ataxia_set_chance (Chance, Params) ->
   {
      set_chance(Chance, Params),
      ataxic:update_field(#btl_cond_params.chance, ataxic:constant(Chance))
   }.

%%%%%%%%%%%%%%%
%%%% Other %%%%
%%%%%%%%%%%%%%%
-spec get_other (type(ODT)) -> ODT.
get_other (Params) -> Params#btl_cond_params.other.

-spec set_other (ODT, type(ODT)) -> type(ODT).
set_other (Other, Params) -> Params#btl_cond_params{ other = Other }.

-spec ataxia_set_other (ODT, type(ODT)) -> {type(ODT), ataxic:basic()}.
ataxia_set_other (Other, Params) ->
   ataxia_set_other(Other, ataxic:constant(Other), Params).

-spec ataxia_set_other
   (
      ODT,
      ataxic:basic(),
      type(ODT)
   )
   -> {type(ODT), ataxic:basic()}.
ataxia_set_other (Other, OtherAtaxicUpdate, Params) ->
   {
      set_other(Other, Params),
      ataxic:update_field(#btl_cond_params.other, OtherAtaxicUpdate)
   }.
