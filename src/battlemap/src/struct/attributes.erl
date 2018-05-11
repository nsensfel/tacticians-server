-module(attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   attributes,
   {
      constitution :: integer(),
      dexterity :: integer(),
      intelligence :: integer(),
      mind :: integer(),
      speed :: integer(),
      strength :: integer()
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
      set_strength/2
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_constitution (type()) -> integer().
get_constitution (Att) -> Att#attributes.constitution.

-spec get_dexterity (type()) -> integer().
get_dexterity (Att) -> Att#attributes.dexterity.

-spec get_intelligence (type()) -> integer().
get_intelligence (Att) -> Att#attributes.intelligence.

-spec get_mind (type()) -> integer().
get_mind (Att) -> Att#attributes.mind.

-spec get_speed (type()) -> integer().
get_speed (Att) -> Att#attributes.speed.

-spec get_strength (type()) -> integer().
get_strength (Att) -> Att#attributes.strength.

-spec set_constitution (integer(), type()) -> type().
set_constitution (Val, Att) -> Att#attributes{ constitution = Val }.

-spec set_dexterity (integer(), type()) -> type().
set_dexterity (Val, Att) -> Att#attributes{ dexterity = Val }.

-spec set_intelligence (integer(), type()) -> type().
set_intelligence (Val, Att) -> Att#attributes{ intelligence = Val }.

-spec set_mind (integer(), type()) -> type().
set_mind (Val, Att) -> Att#attributes{ mind = Val }.

-spec set_speed (integer(), type()) -> type().
set_speed (Val, Att) -> Att#attributes{ speed = Val }.

-spec set_strength (integer(), type()) -> type().
set_strength (Val, Att) -> Att#attributes{ strength = Val }.

-spec random () -> type().
random () ->
   #attributes
   {
      constitution = roll:percentage(),
      dexterity = roll:percentage(),
      intelligence = roll:percentage(),
      mind = roll:percentage(),
      speed = roll:percentage(),
      strength = roll:percentage()
   }.
