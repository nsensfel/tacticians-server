-module(attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   attributes,
   {
      constitution,
      dexterity,
      intelligence,
      mind,
      speed,
      strength
   }
).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_constitution (Att) -> Att#attributes.constitution.
get_dexterity (Att) -> Att#attributes.dexterity.
get_intelligence (Att) -> Att#attributes.intelligence.
get_mind (Att) -> Att#attributes.mind.
get_speed (Att) -> Att#attributes.speed.
get_strength (Att) -> Att#attributes.strength.

set_constitution (Att, Val) ->
   Att#attributes{ constitution = Val }.
set_dexterity (Att, Val) ->
   Att#attributes{ dexterity = Val }.
set_intelligence (Att, Val) ->
   Att#attributes{ intelligence = Val }.
set_mind (Att, Val) ->
   Att#attributes{ mind = Val }.
set_speed (Att, Val) ->
   Att#attributes{ speed = Val }.
set_strength (Att, Val) ->
   Att#attributes{ strength = Val }.
