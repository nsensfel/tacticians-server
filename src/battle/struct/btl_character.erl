-module(btl_character).

-define(PLAYER_IX_FIELD, <<"pla">>).
-define(RANK_FIELD, <<"rnk">>).
-define(LOCATION_FIELD, <<"lc">>).
-define(CURRENT_HEALTH_FIELD, <<"he">>).
-define(SKILL_POINTS_FIELD, <<"sp">>).
-define(IS_ACTIVE_FIELD, <<"ena">>).
-define(IS_DEFEATED_FIELD, <<"dea">>).
-define(BASE_CHAR_FIELD, <<"bas">>).
-define(CONDITIONS_FIELD, <<"con">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type rank() :: ('optional' | 'target' | 'commander').

-record
(
   btl_char_ref,
   {
      player_ix :: non_neg_integer(),
      rank :: rank(),
      location :: {non_neg_integer(), non_neg_integer()},
      current_health :: integer(), %% Negative integers let us reverse attacks.
      skill_points :: integer(), %% Negative integers let us reverse skill uses.
      is_active :: boolean(),
      is_defeated :: boolean(),
      base :: shr_character:unresolved(),
      conditions :: btl_condition:collection()
   }
).

-record
(
   btl_char,
   {
      player_ix :: non_neg_integer(),
      rank :: rank(),
      location :: {non_neg_integer(), non_neg_integer()},
      current_health :: integer(), %% Negative integers let us reverse attacks.
      skill_points :: integer(), %% Negative integers let us reverse skill uses.
      is_active :: boolean(),
      is_defeated :: boolean(),
      base :: shr_character:type(),
      conditions :: btl_condition:collection()
   }
).

-opaque type() :: #btl_char{}.
-opaque unresolved() :: #btl_char_ref{}.
-type either() :: (type() | unresolved()).
-export_type([type/0, unresolved/0, either/0, rank/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_player_index/1,
      get_rank/1,
      get_location/1,
      get_current_health/1,
      get_skill_points/1,
      get_is_alive/1,
      get_is_active/1,
      get_is_defeated/1,
      get_base_character/1,
      get_conditions/1,

      set_rank/2,
      set_location/3,
      set_current_health/2,
      set_skill_points/2,
      set_is_active/2,
      set_is_defeated/2,
      set_base_character/2,
      set_conditions/2,

      ataxia_set_rank/2,
      ataxia_set_location/3,
      ataxia_set_current_health/2,
      ataxia_set_skill_points/2,
      ataxia_set_is_active/2,
      ataxia_set_is_defeated/2,
      ataxia_set_base_character/2,
      ataxia_set_conditions/2,

      ataxia_set_conditions/3,
      ataxia_set_base_character/3,

      get_rank_field/0,
      get_current_health_field/0,
      get_skill_points_field/0,
      get_is_active_field/0,
      get_is_defeated_field/0,
      get_location_field/0,
      get_base_character_field/0,
      get_conditions_field/0
   ]
).

-export
(
   [
      new/5,
      resolve/2,
      is_unresolved/1,
      to_unresolved/1,
      encode/1
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
%%%% Accessors
-spec get_player_index (either()) -> non_neg_integer().
get_player_index (#btl_char{ player_ix = R }) -> R;
get_player_index (#btl_char_ref{ player_ix = R }) -> R.

-spec get_rank (either()) -> rank().
get_rank (#btl_char{ rank = R }) -> R;
get_rank (#btl_char_ref{ rank = R }) -> R.

-spec get_location (either()) -> {non_neg_integer(), non_neg_integer()}.
get_location (#btl_char{ location = R }) -> R;
get_location (#btl_char_ref{ location = R }) -> R.

-spec get_current_health (either()) -> integer().
get_current_health (#btl_char{ current_health = R }) -> R;
get_current_health (#btl_char_ref{ current_health = R }) -> R.

-spec get_skill_points (either()) -> integer().
get_skill_points (#btl_char{ skill_points = R }) -> R;
get_skill_points (#btl_char_ref{ skill_points = R }) -> R.

-spec get_is_alive (either()) -> boolean().
get_is_alive (#btl_char{ current_health = H, is_defeated = D }) ->
   ((not D) and (H > 0));
get_is_alive (#btl_char_ref{ current_health = H, is_defeated = D }) ->
   ((not D) and (H > 0)).

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

-spec get_is_defeated (either()) -> boolean().
get_is_defeated (#btl_char{ is_defeated = R }) -> R;
get_is_defeated (#btl_char_ref{ is_defeated = R }) -> R.

-spec get_base_character
   (type()) -> shr_character:type();
   (unresolved()) -> shr_character:unresolved().
get_base_character (#btl_char{ base = R }) -> R;
get_base_character (#btl_char_ref{ base = R }) -> R.

-spec get_conditions
   (type()) -> btl_condition:collection();
   (unresolved()) -> btl_condition:collection().
get_conditions (#btl_char{ conditions = R }) -> R;
get_conditions (#btl_char_ref{ conditions = R }) -> R.

-spec set_rank
   (rank(), type()) -> type();
   (rank(), unresolved()) -> unresolved().
set_rank (Rank, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ rank = Rank };
set_rank (Rank, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ rank = Rank }.

-spec ataxia_set_rank
   (rank(), type()) -> {type(), ataxic:basic()};
   (rank(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_rank (Rank, Char) ->
   {
      set_rank(Rank, Char),
      ataxic:update_field
      (
         get_rank_field(),
         ataxic:constant(Rank)
      )
   }.

-spec set_location
   (
      {non_neg_integer(), non_neg_integer()},
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
      {non_neg_integer(), non_neg_integer()},
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

-spec set_conditions
   (
      btl_condition:collection(),
      type()
   )
   -> type();
   (
      btl_condition:collection(),
      unresolved()
   )
   -> unresolved().
set_conditions (Conditions, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ conditions = Conditions };
set_conditions (Conditions, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ conditions = Conditions }.


-spec ataxia_set_conditions
   (
      btl_condition:collection(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()};
   (
      btl_condition:collection(),
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
   (
      btl_condition:collection(),
      type()
   )
   -> {type(), ataxic:basic()};
   (
      btl_condition:collection(),
      unresolved()
   )
   -> {unresolved(), ataxic:basic()}.
ataxia_set_conditions (Conditions, Char) ->
   ataxia_set_conditions
   (
      Conditions,
      ataxic:constant(Conditions),
      Char
   ).

%%%% Utils
-spec new
   (
      non_neg_integer(),
      rank(),
      shr_location:type(),
      shr_character:type(),
      btl_condition:collection()
   )
   -> type().
new
(
   PlayerIX,
   Rank,
   Location,
   Base,
   Conditions
) ->
   Attributes = shr_character:get_attributes(Base),

   #btl_char
   {
      player_ix = PlayerIX,
      rank = Rank,
      location = Location,
      current_health = shr_attributes:get_health(Attributes),
      skill_points = 0,
      is_active = (PlayerIX == 0),
      is_defeated = false,
      base = Base,
      conditions = Conditions
   }.

-spec resolve (shr_omnimods:type(), either()) -> type().
resolve (LocalOmnimods, CharRef) when is_record(CharRef, btl_char_ref) ->
   #btl_char
   {
      player_ix = CharRef#btl_char_ref.player_ix,
      rank = CharRef#btl_char_ref.rank,
      location = CharRef#btl_char_ref.location,
      current_health = CharRef#btl_char_ref.current_health,
      skill_points = CharRef#btl_char_ref.skill_points,
      is_active = CharRef#btl_char_ref.is_active,
      is_defeated = CharRef#btl_char_ref.is_defeated,
      base = shr_character:resolve(LocalOmnimods, CharRef#btl_char_ref.base),
      conditions = CharRef#btl_char_ref.conditions
   };
resolve (_LocalOmnimods, Char) when is_record(Char, btl_char) -> Char.

-spec to_unresolved (either()) -> unresolved().
to_unresolved (Char) when is_record(Char, btl_char) ->
   #btl_char_ref
   {
      player_ix = Char#btl_char.player_ix,
      rank = Char#btl_char.rank,
      location = Char#btl_char.location,
      current_health = Char#btl_char.current_health,
      skill_points = Char#btl_char.skill_points,
      is_active = Char#btl_char.is_active,
      is_defeated = Char#btl_char.is_defeated,
      base = shr_character:to_unresolved(Char#btl_char.base),
      conditions = Char#btl_char.conditions
   };
to_unresolved (CharRef) when is_record(CharRef, btl_char_ref) -> CharRef.

-spec is_unresolved (either()) -> boolean().
is_unresolved (Char) -> is_record(Char, btl_char_ref).

-spec get_rank_field() -> non_neg_integer().
get_rank_field () -> #btl_char_ref.rank.
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

-spec encode (unresolved()) -> {list({binary(), any()})}.
encode (CharRef) ->
   {
      [
         {?PLAYER_IX_FIELD, CharRef#btl_char_ref.player_ix},
         {?RANK_FIELD, CharRef#btl_char_ref.rank},
         {?LOCATION_FIELD, shr_location:encode(CharRef#btl_char_ref.location)},
         {?CURRENT_HEALTH_FIELD, CharRef#btl_char_ref.current_health},
         {?SKILL_POINTS_FIELD, CharRef#btl_char_ref.skill_points},
         {?IS_ACTIVE_FIELD, CharRef#btl_char_ref.is_active},
         {?IS_DEFEATED_FIELD, CharRef#btl_char_ref.is_defeated},
         {?BASE_CHAR_FIELD, shr_character:encode(CharRef#btl_char_ref.base)},
         {
            ?CONDITIONS_FIELD,
            btl_condition:encode_collection(CharRef#btl_char_ref.conditions)
         }
      ]
   }.
