-module(shr_attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   attributes,
   {
      constitution :: non_neg_integer(),
      dexterity :: non_neg_integer(),
      intelligence :: non_neg_integer(),
      mind :: non_neg_integer(),
      speed :: non_neg_integer(),
      strength :: non_neg_integer()
   }
).

-opaque type() :: #attributes{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_constitution/1,
      get_dexterity/1,
      get_intelligence/1,
      get_mind/1,
      get_speed/1,
      get_strength/1,

      set_constitution/2,
      set_dexterity/2,
      set_intelligence/2,
      set_mind/2,
      set_speed/2,
      set_strength/2,

      set_unsafe_constitution/2,
      set_unsafe_dexterity/2,
      set_unsafe_intelligence/2,
      set_unsafe_mind/2,
      set_unsafe_speed/2,
      set_unsafe_strength/2
   ]
).

%%%% Accessors
-export
(
   [
      random/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec make_safe (integer()) -> non_neg_integer().
make_safe (Val) -> max(0, min(100, Val)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_constitution (type()) -> non_neg_integer().
get_constitution (Att) -> Att#attributes.constitution.

-spec get_dexterity (type()) -> non_neg_integer().
get_dexterity (Att) -> Att#attributes.dexterity.

-spec get_intelligence (type()) -> non_neg_integer().
get_intelligence (Att) -> Att#attributes.intelligence.

-spec get_mind (type()) -> non_neg_integer().
get_mind (Att) -> Att#attributes.mind.

-spec get_speed (type()) -> non_neg_integer().
get_speed (Att) -> Att#attributes.speed.

-spec get_strength (type()) -> non_neg_integer().
get_strength (Att) -> Att#attributes.strength.

-spec set_constitution (non_neg_integer(), type()) -> type().
set_constitution (Val, Att) -> Att#attributes{ constitution = Val }.

-spec set_dexterity (non_neg_integer(), type()) -> type().
set_dexterity (Val, Att) -> Att#attributes{ dexterity = Val }.

-spec set_intelligence (non_neg_integer(), type()) -> type().
set_intelligence (Val, Att) -> Att#attributes{ intelligence = Val }.

-spec set_mind (non_neg_integer(), type()) -> type().
set_mind (Val, Att) -> Att#attributes{ mind = Val }.

-spec set_speed (non_neg_integer(), type()) -> type().
set_speed (Val, Att) -> Att#attributes{ speed = Val }.

-spec set_strength (non_neg_integer(), type()) -> type().
set_strength (Val, Att) -> Att#attributes{ strength = Val }.

-spec set_unsafe_constitution (integer(), type()) -> type().
set_unsafe_constitution (Val, Att) -> set_constitution(make_safe(Val), Att).

-spec set_unsafe_dexterity (integer(), type()) -> type().
set_unsafe_dexterity (Val, Att) -> set_dexterity(make_safe(Val), Att).

-spec set_unsafe_intelligence (integer(), type()) -> type().
set_unsafe_intelligence (Val, Att) -> set_intelligence(make_safe(Val), Att).

-spec set_unsafe_mind (integer(), type()) -> type().
set_unsafe_mind (Val, Att) -> set_mind(make_safe(Val), Att).

-spec set_unsafe_speed (integer(), type()) -> type().
set_unsafe_speed (Val, Att) -> set_speed(make_safe(Val), Att).

-spec set_unsafe_strength (integer(), type()) -> type().
set_unsafe_strength (Val, Att) -> set_strength(make_safe(Val), Att).

-spec random () -> type().
random () ->
   #attributes
   {
      constitution = shr_roll:percentage(),
      dexterity = shr_roll:percentage(),
      intelligence = shr_roll:percentage(),
      mind = shr_roll:percentage(),
      speed = shr_roll:percentage(),
      strength = shr_roll:percentage()
   }.
