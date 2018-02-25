-module(add_char).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attributes_as_json (Attributes) ->
   {
      [
         {<<"con">>, attributes:get_constitution(Attributes)},
         {<<"dex">>, attributes:get_dexterity(Attributes)},
         {<<"int">>, attributes:get_intelligence(Attributes)},
         {<<"min">>, attributes:get_mind(Attributes)},
         {<<"spe">>, attributes:get_speed(Attributes)},
         {<<"str">>, attributes:get_strength(Attributes)}
      ]
   }.

encode (IX, CharacterInstance) ->
   Character = character_instance:get_character(CharacterInstance),
   {X, Y} = character_instance:get_location(CharacterInstance),
   Attributes = character:get_attributes(Character),
   {ActiveWeapon, SecondaryWeapon} = character:get_weapons(Character),

   jiffy:encode
   (
      {
         [
            {<<"ix">>, IX},
            {<<"nam">>, character:get_name(Character)},
            {<<"ico">>, character:get_icon(Character)},
            {<<"prt">>, character:get_portrait(Character)},
            {
               <<"hea">>,
               character_instance:get_current_health(CharacterInstance)
            },
            {<<"lcx">>, X},
            {<<"lcy">>, Y},
            {<<"tem">>, character:get_owner_id(Character)},
            {<<"ena">>, character_instance:get_is_active(CharacterInstance)},
            {<<"att">>, attributes_as_json(Attributes)},
            {<<"awp">>, ActiveWeapon},
            {<<"swp">>, SecondaryWeapon}
         ]
      }
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate (IX, CharacterInstance) ->
   [<<"add_char">>, encode(IX, CharacterInstance)].
