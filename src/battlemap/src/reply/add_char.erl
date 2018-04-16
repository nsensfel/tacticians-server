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
-spec attributes_as_json
   (
      attributes:struct()
   ) ->
   {list({binary(), non_neg_integer()})}.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate
   (
      non_neg_integer(),
      character_instance:struct(),
      player:id()
   )
   -> {list(any())}.
generate (IX, CharacterInstance, PlayerID) ->
   Character = character_instance:get_character(CharacterInstance),
   IsAlive = character_instance:get_is_alive(CharacterInstance),
   Attributes = character:get_attributes(Character),
   {ActiveWeapon, SecondaryWeapon} = character:get_weapon_ids(Character),
   OwnerID = character:get_owner_id(Character),
   Location =
      case IsAlive of
         true -> character_instance:get_location(CharacterInstance);
         _ -> location:get_nowhere()
      end,

   {
      [
         {<<"msg">>, <<"add_char">>},
         {<<"ix">>, IX},
         {<<"nam">>, character:get_name(Character)},
         {<<"ico">>, character:get_icon(Character)},
         {<<"prt">>, character:get_portrait(Character)},
         {
            <<"hea">>,
            character_instance:get_current_health(CharacterInstance)
         },
         {<<"lc">>, location:encode(Location)},
         {<<"pla">>, OwnerID},
         {
            <<"ena">>,
            (
               character_instance:get_is_active(CharacterInstance)
               and
               (OwnerID == PlayerID)
            )
         },
         {<<"att">>, attributes_as_json(Attributes)},
         {<<"awp">>, ActiveWeapon},
         {<<"swp">>, SecondaryWeapon}
      ]
   }.
