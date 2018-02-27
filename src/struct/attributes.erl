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

-opaque struct() :: #attributes{}.

-export_type([struct/0]).

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
-spec get_constitution (struct()) -> integer().
get_constitution (Att) -> Att#attributes.constitution.

-spec get_dexterity (struct()) -> integer().
get_dexterity (Att) -> Att#attributes.dexterity.

-spec get_intelligence (struct()) -> integer().
get_intelligence (Att) -> Att#attributes.intelligence.

-spec get_mind (struct()) -> integer().
get_mind (Att) -> Att#attributes.mind.

-spec get_speed (struct()) -> integer().
get_speed (Att) -> Att#attributes.speed.

-spec get_strength (struct()) -> integer().
get_strength (Att) -> Att#attributes.strength.

-spec set_constitution (integer(), struct()) -> struct().
set_constitution (Val, Att) -> Att#attributes{ constitution = Val }.

-spec set_dexterity (integer(), struct()) -> struct().
set_dexterity (Val, Att) -> Att#attributes{ dexterity = Val }.

-spec set_intelligence (integer(), struct()) -> struct().
set_intelligence (Val, Att) -> Att#attributes{ intelligence = Val }.

-spec set_mind (integer(), struct()) -> struct().
set_mind (Val, Att) -> Att#attributes{ mind = Val }.

-spec set_speed (integer(), struct()) -> struct().
set_speed (Val, Att) -> Att#attributes{ speed = Val }.

-spec set_strength (integer(), struct()) -> struct().
set_strength (Val, Att) -> Att#attributes{ strength = Val }.

-spec random () -> struct().
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
