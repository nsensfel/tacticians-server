-module(add_char).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attributes_as_json (Atts) ->
   {
      [
         {<<"con">>, attributes:get_constitution(Atts)},
         {<<"dex">>, attributes:get_dexterity(Atts)},
         {<<"int">>, attributes:get_intelligence(Atts)},
         {<<"min">>, attributes:get_mind(Atts)},
         {<<"spe">>, attributes:get_speed(Atts)},
         {<<"str">>, attributes:get_strength(Atts)}
      ]
   }.

encode (Char, CharInstance, IsEnabled) ->
   {X, Y} = character_instance:get_location(CharInstance),
   Atts = character:get_attributes(Char),
   ActWeapon = character_instance:get_active_weapon(CharInstance, Char),
   {_MinRg, MaxRg} = weapon:get_ranges(ActWeapon),
   jiffy:encode
   (
      {
         [
            {<<"id">>, character:get_id(Char)},
            {<<"name">>, character:get_name(Char)},
            {<<"icon">>, character:get_icon(Char)},
            {<<"portrait">>, character:get_portrait(Char)},
            {<<"health">>, character_instance:get_current_health(CharInstance)},
            {<<"loc_x">>, X},
            {<<"loc_y">>, Y},
            {<<"team">>, character_instance:get_owner(CharInstance)},
            {<<"enabled">>, IsEnabled},
            {<<"att">>, attributes_as_json(Atts)},
            {<<"atk_rg">>, MaxRg}
         ]
      }
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate (Char, CharInstance, IsEnabled) ->
   [<<"add_char">>, encode(Char, CharInstance, IsEnabled)].
