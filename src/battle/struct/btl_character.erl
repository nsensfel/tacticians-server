-module(btl_character).

-define(PLAYER_IX_FIELD, <<"pla">>).
-define(RANK_FIELD, <<"rnk">>).
-define(LOCATION_FIELD, <<"lc">>).
-define(CURRENT_HEALTH_FIELD, <<"he">>).
-define(IS_ACTIVE_FIELD, <<"ena">>).
-define(IS_DEFEATED_FIELD, <<"dea">>).
-define(BASE_CHAR_FIELD, <<"bas">>).

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
      is_active :: boolean(),
      is_defeated :: boolean(),
      base :: shr_character:unresolved()
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
      is_active :: boolean(),
      is_defeated :: boolean(),
      base :: shr_character:type()
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
      get_is_alive/1,
      get_is_active/1,
      get_is_defeated/1,
      get_base_character/1,

      set_rank/2,
      set_location/2,
      set_current_health/2,
      set_is_active/2,
      set_is_defeated/2,
      set_base_character/2,

      ataxia_set_rank/2,
      ataxia_set_location/2,
      ataxia_set_current_health/2,
      ataxia_set_is_active/2,
      ataxia_set_is_defeated/2,
      ataxia_set_base_character/2,

      ataxia_set_base_character/3,

      get_rank_field/0,
      get_current_health_field/0,
      get_is_active_field/0,
      get_is_defeated_field/0,
      get_location_field/0,
      get_base_character_field/0
   ]
).

-export
(
   [
      new/4,
      resolve/2,
      to_unresolved/1,
      decode/1,
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_player_index (either()) -> non_neg_integer().
get_player_index (#btl_char{ player_ix = R}) -> R;
get_player_index (#btl_char_ref{ player_ix = R}) -> R.

-spec get_rank (either()) -> rank().
get_rank (#btl_char{ rank = R }) -> R;
get_rank (#btl_char_ref{ rank = R }) -> R.

-spec get_location (either()) -> {non_neg_integer(), non_neg_integer()}.
get_location (#btl_char{ location = R }) -> R;
get_location (#btl_char_ref{ location = R }) -> R.

-spec get_current_health (either()) -> integer().
get_current_health (#btl_char{ current_health = R }) -> R;
get_current_health (#btl_char_ref{ current_health = R }) -> R.

-spec get_is_alive (type()) -> boolean().
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

-spec set_rank
   (rank(), type()) -> type();
   (rank(), unresolved()) -> unresolved().
set_rank (Rank, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ rank = Rank };
set_rank (Rank, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ rank = Rank }.

% TODO: This can change current_health.
% FIXME: Can't do this without giving the new tile omnimods.
-spec set_location
   (
      {non_neg_integer(), non_neg_integer()},
      shr_omnimods:type(),
      type()
   )
   -> type();
set_location (Location, LocOmnimods, Char) ->
   BaseCharacter = 
   CurrentMaxHealth =
      shr_statistics:get_health
      (
         shr_character:get_statistics
      )
   Char#btl_char{ location = Location }.

-spec set_current_health
   (integer(), type()) -> type();
   (integer(), unresolved()) -> unresolved().
set_current_health (Health, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ current_health = Health };
set_current_health (Health, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ current_health = Health }.

-spec set_is_active
   (boolean(), type()) -> type();
   (boolean(), unresolved()) -> unresolved().
set_is_active (Active, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ is_active = Active };
set_is_active (Active, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ is_active = Active }.

-spec set_is_defeated
   (boolean(), type()) -> type();
   (boolean(), unresolved()) -> unresolved().
set_is_defeated (Defeated, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ is_defeated = Defeated };
set_is_defeated (Defeated, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ is_defeated = Defeated }.

% TODO: This can change current_health.
-spec set_base_character
   (shr_character:type(), type()) -> type();
   (shr_character:unresolved(), unresolved()) -> unresolved().
set_base_character (Base, Char) when is_record(Char, btl_char) ->
   Char#btl_char{ base = Base };
set_base_character (Base, Char) when is_record(Char, btl_char_ref) ->
   Char#btl_char_ref{ base = Base }.

%%%% Utils
-spec new
   (
      non_neg_integer(),
      rank(),
      shr_location:type(),
      shr_character:type()
   )
   -> type().
new
(
   PlayerIX,
   Rank,
   Location,
   Base
) ->
   Statistics = shr_character:get_statistics(Base),

   #btl_char
   {
      player_ix = PlayerIX,
      rank = Rank,
      location = Location,
      current_health = shr_statistics:get_health(Statistics),
      is_active = (PlayerIX == 0),
      is_defeated = false,
      base = Base
   }.

-spec resolve (shr_omnimods:type(), unresolved()) -> type().
resolve (LocalOmnimods, CharRef) ->
   #btl_char
   {
      player_ix = CharRef#btl_char_ref.player_ix,
      rank = CharRef#btl_char_ref.rank,
      location = CharRef#btl_char_ref.location,
      current_health = CharRef#btl_char_ref.current_health,
      is_active = CharRef#btl_char_ref.is_active,
      is_defeated = CharRef#btl_char_ref.is_defeated,
      base = shr_character:resolve(LocalOmnimods, CharRef#btl_char_ref.base)
   }.

-spec to_unresolved (type()) -> unresolved().
to_unresolved (Char) ->
   #btl_char_ref
   {
      player_ix = Char#btl_char.player_ix,
      rank = Char#btl_char.rank,
      location = Char#btl_char.location,
      current_health = Char#btl_char.current_health,
      is_active = Char#btl_char.is_active,
      is_defeated = Char#btl_char.is_defeated,
      base = shr_character:to_unresolved(Char#btl_char.base)
   }.

-spec get_rank_field() -> non_neg_integer().
get_rank_field () -> #btl_char_ref.rank.
-spec get_location_field() -> non_neg_integer().
get_location_field () -> #btl_char_ref.location.
-spec get_current_health_field() -> non_neg_integer().
get_current_health_field () -> #btl_char_ref.current_health.
-spec get_is_active_field() -> non_neg_integer().
get_is_active_field () -> #btl_char_ref.is_active.
-spec get_is_defeated_field() -> non_neg_integer().
get_is_defeated_field () -> #btl_char_ref.is_defeated.
-spec get_base_character_field() -> non_neg_integer().
get_base_character_field () -> #btl_char_ref.base.

-spec decode (map()) -> unresolved().
decode (Map) ->
   #btl_char_ref
   {
      player_ix =  maps:get(?PLAYER_IX_FIELD, Map),
      rank = maps:get(?RANK_FIELD, Map),
      location = shr_location:decode(maps:get(?LOCATION_FIELD, Map)),
      current_health = maps:get(?CURRENT_HEALTH_FIELD, Map),
      is_active = maps:get(?IS_ACTIVE_FIELD, Map),
      is_defeated = maps:get(?IS_DEFEATED_FIELD, Map),
      base = shr_character:decode(maps:get(?BASE_CHAR_FIELD, Map))
   }.

-spec encode (unresolved()) -> {list({binary(), any()})}.
encode (CharRef) ->
   {
      [
         {?PLAYER_IX_FIELD, CharRef#btl_char_ref.player_ix},
         {?RANK_FIELD, CharRef#btl_char_ref.rank},
         {?LOCATION_FIELD, shr_location:encode(CharRef#btl_char_ref.location)},
         {?CURRENT_HEALTH_FIELD, CharRef#btl_char_ref.current_health},
         {?IS_ACTIVE_FIELD, CharRef#btl_char_ref.is_active},
         {?IS_DEFEATED_FIELD, CharRef#btl_char_ref.is_defeated},
         {?BASE_CHAR_FIELD, shr_character:encode(CharRef#btl_char_ref.base)}
      ]
   }.
