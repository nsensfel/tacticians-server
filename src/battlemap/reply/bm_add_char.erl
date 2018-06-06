-module(bm_add_char).

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
      sh_attributes:type()
   ) ->
   {list({binary(), non_neg_integer()})}.
attributes_as_json (Attributes) ->
   {
      [
         {<<"con">>, sh_attributes:get_constitution(Attributes)},
         {<<"dex">>, sh_attributes:get_dexterity(Attributes)},
         {<<"int">>, sh_attributes:get_intelligence(Attributes)},
         {<<"min">>, sh_attributes:get_mind(Attributes)},
         {<<"spe">>, sh_attributes:get_speed(Attributes)},
         {<<"str">>, sh_attributes:get_strength(Attributes)}
      ]
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate
   (
      non_neg_integer(),
      bm_character:type(),
      bm_player:id()
   )
   -> {list(any())}.
generate (IX, Character, PlayerID) ->
   IsAlive = bm_character:get_is_alive(Character),
   Attributes = bm_character:get_attributes(Character),
   {ActiveWeapon, SecondaryWeapon} = bm_character:get_weapon_ids(Character),
   OwnerID = bm_character:get_owner_id(Character),
   Location =
      case IsAlive of
         true -> bm_character:get_location(Character);
         _ -> bm_location:get_nowhere()
      end,

   {
      [
         {<<"msg">>, <<"add_char">>},
         {<<"ix">>, IX},
         {<<"nam">>, bm_character:get_name(Character)},
         {<<"ico">>, bm_character:get_icon(Character)},
         {<<"prt">>, bm_character:get_portrait(Character)},
         {
            <<"hea">>,
            bm_character:get_current_health(Character)
         },
         {<<"lc">>, bm_location:encode(Location)},
         {<<"pla">>, OwnerID},
         {
            <<"ena">>,
            (
               bm_character:get_is_active(Character)
               and
               (OwnerID == PlayerID)
            )
         },
         {<<"att">>, attributes_as_json(Attributes)},
         {<<"awp">>, ActiveWeapon},
         {<<"swp">>, SecondaryWeapon}
      ]
   }.
