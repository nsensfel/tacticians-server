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
encode (Char, CharInstance, IsEnabled) ->
   {X, Y} = character_instance:get_location(CharInstance),
   Stats = character_instance:get_statistics(CharInstance),
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
            {<<"max_health">>, statistics:get_health(Stats)},
            {<<"mov_pts">>, statistics:get_movement_points(Stats)},
            {<<"atk_rg">>, MaxRg},
            {<<"enabled">>, IsEnabled}
         ]
      }
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate (Char, CharInstance, IsEnabled) ->
   [<<"add_char">>, encode(Char, CharInstance, IsEnabled)].
