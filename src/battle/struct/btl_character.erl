-module(btl_character).

-define(PLAYER_IX_FIELD, <<"pla">>).
-define(LOCATION_FIELD, <<"lc">>).
-define(CURRENT_HEALTH_FIELD, <<"he">>).
-define(SKILL_POINTS_FIELD, <<"sp">>).
-define(IS_ACTIVE_FIELD, <<"ena">>).
-define(IS_DEFEATED_FIELD, <<"dea">>).
-define(BASE_CHAR_FIELD, <<"bas">>).
-define(STATUS_INDICATORS, <<"sti">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   btl_char_ref,
   {
      player_ix :: non_neg_integer(),
      location :: shr_location:type(),
      current_health :: integer(), %% Negative integers let us reverse attacks.
      skill_points :: integer(), %% Negative integers let us reverse skill uses.
      is_active :: boolean(),
      is_defeated :: boolean(),
      base :: shr_character:unresolved(),
      conditions :: btl_conditions:type(),
      status_indicators :: btl_status_indicators:type()
   }
).

-record
(
   btl_char,
   {
      player_ix :: non_neg_integer(),
      location :: shr_location:type(),
      current_health :: integer(), %% Negative integers let us reverse attacks.
      skill_points :: integer(), %% Negative integers let us reverse skill uses.
      is_active :: boolean(),
      is_defeated :: boolean(),
      base :: shr_character:type(),
      conditions :: btl_conditions:type(),
      status_indicators :: btl_status_indicators:type()
   }
).

-opaque type() :: #btl_char{}.
-opaque unresolved() :: #btl_char_ref{}.
-type either() :: (type() | unresolved()).

-export_type([type/0, unresolved/0, either/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_player_index/1,
      get_location/1,
      get_current_health/1,
      get_skill_points/1,
      get_is_alive/1,
      get_is_active/1,
      get_is_defeated/1,
      get_base_character/1,
      get_conditions/1,
      get_status_indicators/1,

      set_location/3,
      set_current_health/2,
      set_skill_points/2,
      set_is_active/2,
      set_is_defeated/2,
      set_base_character/2,
      set_conditions/2,
      set_status_indicators/2,

      ataxia_set_location/3,
      ataxia_set_current_health/2,
      ataxia_set_skill_points/2,
      ataxia_set_is_active/2,
      ataxia_set_is_defeated/2,
      ataxia_set_base_character/2,
      ataxia_set_conditions/2,
      ataxia_set_status_indicators/2,

      ataxia_set_conditions/3,
      ataxia_set_status_indicators/3,
      ataxia_set_base_character/3,

      get_current_health_field/0,
      get_skill_points_field/0,
      get_is_active_field/0,
      get_is_defeated_field/0,
      get_location_field/0,
      get_base_character_field/0,
      get_conditions_field/0,
      get_status_indicators_field/0
   ]
).

-export
(
   [
      new/5,
      resolve/2,
      is_unresolved/1,
      to_unresolved/1,
      encode_for/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_max_health_change
   (
      shr_character:type(),
      shr_character:type(),
      integer()
   )
   -> {boolean(), integer()}.
handle_max_health_change (OldBaseChar, NewBaseChar, OldHealth) ->
   OldMaxHealth =
      shr_attributes:get_health(shr_character:get_attributes(OldBaseChar)),

   NewMaxHealth =
      shr_attributes:get_health(shr_character:get_attributes(NewBaseChar)),

   case (OldMaxHealth == NewMaxHealth) of
      true -> {false, OldHealth};
      false ->
         OldHealthRatio = (OldHealth / OldMaxHealth),
         NewHealth =
            min
            (
               NewMaxHealth,
               shr_math_util:ceil(OldHealthRatio * NewMaxHealth)
            ),

         {true, NewHealth}
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Accessors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%
%%%% Player Index %%%%
%%%%%%%%%%%%%%%%%%%%%%
-spec get_player_index (either()) -> non_neg_integer().
get_player_index (#btl_char{ player_ix = R }) -> R;
get_player_index (#btl_char_ref{ player_ix = R }) -> R.

%%%%%%%%%%%%%%%%%%
%%%% Location %%%%
%%%%%%%%%%%%%%%%%%
-spec get_location (either()) -> shr_location:type().
get_location (#btl_char{ location = R }) -> R;
get_location (#btl_char_ref{ location = R }) -> R.

-spec set_location
   (
      shr_location:type(),
      shr_omnimods:type(),
      type()
   )
   -> type().
set_location (Location, LocOmnimods, Char) ->
   CurrentBaseCharacter = Char#btl_char.base,
   UpdatedBaseCharacter =
      shr_character:set_extra_omnimods(LocOmnimods, CurrentBaseCharacter),

   case
      handle_max_health_change
      (
         CurrentBaseCharacter,
         UpdatedBaseCharacter,
         Char#btl_char.current_health
      )
   of
      {false, _} ->
         Char#btl_char
         {
            location = Location,
            base = UpdatedBaseCharacter
         };

      {true, NewHealth} ->
         Char#btl_char
         {
            location = Location,
            base = UpdatedBaseCharacter,
            current_health = NewHealth
         }
   end.

-spec ataxia_set_location
   (
      shr_location:type(),
      shr_omnimods:type(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_location (Location, LocOmnimods, Char) ->
   CurrentHealth = Char#btl_char.current_health,
   UpdatedChar = set_location(Location, LocOmnimods, Char),
   UpdatedCharHealth = UpdatedChar#btl_char.current_health,
   LocationUpdate =
      ataxic:update_field
      (
         get_location_field(),
         ataxic:constant(Location)
      ),

   {
      UpdatedChar,
      case (CurrentHealth == UpdatedCharHealth) of
         true -> LocationUpdate;
         false ->
            ataxic:sequence
            (
               [
                  ataxic:update_field
                  (
                     get_current_health_field(),
                     ataxic:constant(UpdatedCharHealth)
                  ),
                  LocationUpdate
               ]
            )
      end
   }.

%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Current Health %%%%
%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_current_health (either()) -> integer().
get_current_health (#btl_char{ current_health = R }) -> R;
get_current_health (#btl_char_ref{ current_health = R }) -> R.

-spec get_is_alive (either()) -> boolean().
get_is_alive (#btl_char{ current_health = H, is_defeated = D }) ->
   ((not D) and (H > 0));
get_is_alive (#btl_char_ref{ current_health = H, is_defeated = D }) ->
   ((not D) and (H > 0)).

-spec set_current_health
   (integer(), type()) -> type();
   (integer(), unresolved()) -> unresolved().
set_current_health (Health, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ current_health = Health };
set_current_health (Health, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ current_health = Health }.

-spec ataxia_set_current_health
   (integer(), type()) -> {type(), ataxic:basic()};
   (integer(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_current_health (Health, Char) ->
   {
      set_current_health(Health, Char),
      ataxic:update_field
      (
         get_current_health_field(),
         ataxic:constant(Health)
      )
   }.

%%%%%%%%%%%%%%%%%%%%%%
%%%% Skill Points %%%%
%%%%%%%%%%%%%%%%%%%%%%
-spec get_skill_points (either()) -> integer().
get_skill_points (#btl_char{ skill_points = R }) -> R;
get_skill_points (#btl_char_ref{ skill_points = R }) -> R.

-spec set_skill_points
   (integer(), type()) -> type();
   (integer(), unresolved()) -> unresolved().
set_skill_points (SkillPoints, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ skill_points = SkillPoints };
set_skill_points (SkillPoints, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ skill_points = SkillPoints }.

-spec ataxia_set_skill_points
   (integer(), type()) -> {type(), ataxic:basic()};
   (integer(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_skill_points (SkillPoints, Char) ->
   {
      set_skill_points(SkillPoints, Char),
      ataxic:update_field
      (
         get_skill_points_field(),
         ataxic:constant(SkillPoints)
      )
   }.

%%%%%%%%%%%%%%%%%%%
%%%% Is Active %%%%
%%%%%%%%%%%%%%%%%%%
-spec get_is_active (either()) -> boolean().
get_is_active
(
   #btl_char{ current_health = H, is_defeated = D, is_active = A }
) ->
   ((not D) and (H > 0) and A);
get_is_active
(
   #btl_char_ref{ current_health = H, is_defeated = D, is_active = A }
) ->
   ((not D) and (H > 0) and A).

-spec set_is_active
   (boolean(), type()) -> type();
   (boolean(), unresolved()) -> unresolved().
set_is_active (Active, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ is_active = Active };
set_is_active (Active, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ is_active = Active }.

-spec ataxia_set_is_active
   (boolean(), type()) -> {type(), ataxic:basic()};
   (boolean(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_is_active (Active, Char) ->
   {
      set_is_active(Active, Char),
      ataxic:update_field
      (
         get_is_active_field(),
         ataxic:constant(Active)
      )
   }.

%%%%%%%%%%%%%%%%%%%%%
%%%% Is Defeated %%%%
%%%%%%%%%%%%%%%%%%%%%
-spec get_is_defeated (either()) -> boolean().
get_is_defeated (#btl_char{ is_defeated = R }) -> R;
get_is_defeated (#btl_char_ref{ is_defeated = R }) -> R.

-spec set_is_defeated
   (boolean(), type()) -> type();
   (boolean(), unresolved()) -> unresolved().
set_is_defeated (Defeated, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ is_defeated = Defeated };
set_is_defeated (Defeated, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ is_defeated = Defeated }.

-spec ataxia_set_is_defeated
   (boolean(), type()) -> {type(), ataxic:basic()};
   (boolean(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_is_defeated (Defeated, Char) ->
   {
      set_is_defeated(Defeated, Char),
      ataxic:update_field
      (
         get_is_defeated_field(),
         ataxic:constant(Defeated)
      )
   }.

%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Base Character %%%%
%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_base_character
   (type()) -> shr_character:type();
   (unresolved()) -> shr_character:unresolved().
get_base_character (#btl_char{ base = R }) -> R;
get_base_character (#btl_char_ref{ base = R }) -> R.

-spec set_base_character (shr_character:type(), type()) -> type().
set_base_character (NewBaseCharacter, Char) ->
   CurrentBaseCharacter = Char#btl_char.base,
   case
      handle_max_health_change
      (
         CurrentBaseCharacter,
         NewBaseCharacter,
         Char#btl_char.current_health
      )
   of
      {false, _} ->
         Char#btl_char
         {
            base = NewBaseCharacter
         };

      {true, NewHealth} ->
         Char#btl_char
         {
            base = NewBaseCharacter,
            current_health = NewHealth
         }
   end.

-spec ataxia_set_base_character
   (
      shr_character:type(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_base_character (NewBaseCharacter, BaseCharacterAtaxicUpdate, Char) ->
   CurrentHealth = Char#btl_char.current_health,
   UpdatedChar = set_base_character(NewBaseCharacter, Char),
   UpdatedCharHealth = UpdatedChar#btl_char.current_health,
   BattleCharacterAtaxicUpdate =
      ataxic:update_field
      (
         get_base_character_field(),
         BaseCharacterAtaxicUpdate
      ),

   {
      UpdatedChar,
      case (CurrentHealth == UpdatedCharHealth) of
         true -> BattleCharacterAtaxicUpdate;
         false ->
            ataxic:sequence
            (
               [
                  ataxic:update_field
                  (
                     get_current_health_field(),
                     ataxic:constant(UpdatedCharHealth)
                  ),
                  BattleCharacterAtaxicUpdate
               ]
            )
      end
   }.

-spec ataxia_set_base_character
   (
      shr_character:type(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_base_character (NewBaseCharacter, Char) ->
   ataxia_set_base_character
   (
      NewBaseCharacter,
      ataxic:constant(NewBaseCharacter),
      Char
   ).

%%%%%%%%%%%%%%%%%%%%
%%%% Conditions %%%%
%%%%%%%%%%%%%%%%%%%%
-spec get_conditions
   (type()) -> btl_conditions:type();
   (unresolved()) -> btl_conditions:type().
get_conditions (#btl_char{ conditions = R }) -> R;
get_conditions (#btl_char_ref{ conditions = R }) -> R.

-spec set_conditions
   (btl_conditions:type(), type()) -> type();
   (btl_conditions:type(), unresolved()) -> unresolved().
set_conditions (Conditions, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ conditions = Conditions };
set_conditions (Conditions, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ conditions = Conditions }.

-spec ataxia_set_conditions
   (
      btl_conditions:type(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()};
   (
      btl_conditions:type(),
      ataxic:basic(),
      unresolved()
   ) -> {unresolved(), ataxic:basic()}.
ataxia_set_conditions (Conditions, Update, Char) ->
   {
      set_conditions(Conditions, Char),
      ataxic:update_field
      (
         get_conditions_field(),
         Update
      )
   }.

-spec ataxia_set_conditions
   (btl_conditions:type(), type()) -> {type(), ataxic:basic()};
   (btl_conditions:type(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_conditions (Conditions, Char) ->
   ataxia_set_conditions
   (
      Conditions,
      ataxic:constant(Conditions),
      Char
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Status Indicators %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_status_indicators
   (type()) -> btl_status_indicators:type();
   (unresolved()) -> btl_status_indicators:type().
get_status_indicators (#btl_char{ status_indicators = R }) -> R;
get_status_indicators (#btl_char_ref{ status_indicators = R }) -> R.

-spec set_status_indicators
   (btl_status_indicators:type(), type()) -> type();
   (btl_status_indicators:type(), unresolved()) -> unresolved().
set_status_indicators (StatusIndicators, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ status_indicators = StatusIndicators };
set_status_indicators (StatusIndicators, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ status_indicators = StatusIndicators }.

-spec ataxia_set_status_indicators
   (
      btl_status_indicators:type(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()};
   (
      btl_status_indicators:type(),
      ataxic:basic(),
      unresolved()
   ) -> {unresolved(), ataxic:basic()}.
ataxia_set_status_indicators (StatusIndicators, Update, Char) ->
   {
      set_status_indicators(StatusIndicators, Char),
      ataxic:update_field
      (
         get_status_indicators_field(),
         Update
      )
   }.

-spec ataxia_set_status_indicators
   (btl_status_indicators:type(), type()) -> {type(), ataxic:basic()};
   (
      btl_status_indicators:type(),
      unresolved()
   ) -> {unresolved(), ataxic:basic()}.
ataxia_set_status_indicators (StatusIndicators, Char) ->
   ataxia_set_status_indicators
   (
      StatusIndicators,
      ataxic:constant(StatusIndicators),
      Char
   ).

%%%% Utils
-spec new
   (
      non_neg_integer(),
      shr_location:type(),
      shr_character:type(),
      btl_conditions:type(),
      btl_status_indicators:type()
   )
   -> type().
new
(
   PlayerIX,
   Location,
   Base,
   Conditions,
   StatusIndicators
) ->
   Attributes = shr_character:get_attributes(Base),

   #btl_char
   {
      player_ix = PlayerIX,
      location = Location,
      current_health = shr_attributes:get_health(Attributes),
      skill_points = 0,
      is_active = (PlayerIX == 0),
      is_defeated = false,
      base = Base,
      conditions = Conditions,
      status_indicators = StatusIndicators
   }.

-spec resolve (shr_omnimods:type(), either()) -> type().
resolve (LocalOmnimods, CharRef) when is_record(CharRef, btl_char_ref) ->
   #btl_char
   {
      player_ix = CharRef#btl_char_ref.player_ix,
      location = CharRef#btl_char_ref.location,
      current_health = CharRef#btl_char_ref.current_health,
      skill_points = CharRef#btl_char_ref.skill_points,
      is_active = CharRef#btl_char_ref.is_active,
      is_defeated = CharRef#btl_char_ref.is_defeated,
      base = shr_character:resolve(LocalOmnimods, CharRef#btl_char_ref.base),
      conditions = CharRef#btl_char_ref.conditions,
      status_indicators = CharRef#btl_char_ref.status_indicators
   };
resolve (_LocalOmnimods, Char) when is_record(Char, btl_char) -> Char.

-spec to_unresolved (either()) -> unresolved().
to_unresolved (Char) when is_record(Char, btl_char) ->
   #btl_char_ref
   {
      player_ix = Char#btl_char.player_ix,
      location = Char#btl_char.location,
      current_health = Char#btl_char.current_health,
      skill_points = Char#btl_char.skill_points,
      is_active = Char#btl_char.is_active,
      is_defeated = Char#btl_char.is_defeated,
      base = shr_character:to_unresolved(Char#btl_char.base),
      conditions = Char#btl_char.conditions,
      status_indicators = Char#btl_char.status_indicators
   };
to_unresolved (CharRef) when is_record(CharRef, btl_char_ref) -> CharRef.

-spec is_unresolved (either()) -> boolean().
is_unresolved (Char) -> is_record(Char, btl_char_ref).

-spec get_location_field() -> non_neg_integer().
get_location_field () -> #btl_char_ref.location.
-spec get_current_health_field() -> non_neg_integer().
get_current_health_field () -> #btl_char_ref.current_health.
-spec get_skill_points_field() -> non_neg_integer().
get_skill_points_field () -> #btl_char_ref.skill_points.
-spec get_is_active_field() -> non_neg_integer().
get_is_active_field () -> #btl_char_ref.is_active.
-spec get_is_defeated_field() -> non_neg_integer().
get_is_defeated_field () -> #btl_char_ref.is_defeated.
-spec get_base_character_field() -> non_neg_integer().
get_base_character_field () -> #btl_char_ref.base.
-spec get_conditions_field() -> non_neg_integer().
get_conditions_field () -> #btl_char_ref.conditions.
-spec get_status_indicators_field() -> non_neg_integer().
get_status_indicators_field () -> #btl_char_ref.status_indicators.

-spec encode_for (non_neg_integer(), either()) -> {list({binary(), any()})}.
encode_for
   (
      RequestingPlayerIX,
      CharRef
   )
   when is_record(CharRef, btl_char_ref) ->
   {
      [
         {?PLAYER_IX_FIELD, CharRef#btl_char_ref.player_ix},
         {?LOCATION_FIELD, shr_location:encode(CharRef#btl_char_ref.location)},
         {?CURRENT_HEALTH_FIELD, CharRef#btl_char_ref.current_health},
         {?SKILL_POINTS_FIELD, CharRef#btl_char_ref.skill_points},
         {?IS_ACTIVE_FIELD, CharRef#btl_char_ref.is_active},
         {?IS_DEFEATED_FIELD, CharRef#btl_char_ref.is_defeated},
         {?BASE_CHAR_FIELD, shr_character:encode(CharRef#btl_char_ref.base)},
         {
            ?STATUS_INDICATORS,
            btl_status_indicators:encode_for
            (
               RequestingPlayerIX,
               CharRef#btl_char_ref.status_indicators
            )
         }
      ]
   };
encode_for (RequestingPlayerIX, Char) ->
   {
      [
         {?PLAYER_IX_FIELD, Char#btl_char.player_ix},
         {?LOCATION_FIELD, shr_location:encode(Char#btl_char.location)},
         {?CURRENT_HEALTH_FIELD, Char#btl_char.current_health},
         {?SKILL_POINTS_FIELD, Char#btl_char.skill_points},
         {?IS_ACTIVE_FIELD, Char#btl_char.is_active},
         {?IS_DEFEATED_FIELD, Char#btl_char.is_defeated},
         {?BASE_CHAR_FIELD, shr_character:encode(Char#btl_char.base)},
         {
            ?STATUS_INDICATORS,
            btl_status_indicators:encode_for
            (
               RequestingPlayerIX,
               Char#btl_char.status_indicators
            )
         }
      ]
   }.
