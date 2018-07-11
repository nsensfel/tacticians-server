-module(btl_add_char).

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
-spec rank_to_string (btl_character:rank()) -> binary().
rank_to_string (Rank) ->
   case Rank of
      optional -> <<"o">>;
      target -> <<"t">>;
      commander -> <<"c">>
   end.

-spec attributes_as_json
   (
      shr_attributes:type()
   ) ->
   {list({binary(), non_neg_integer()})}.
attributes_as_json (Attributes) ->
   {
      [
         {<<"con">>, shr_attributes:get_constitution(Attributes)},
         {<<"dex">>, shr_attributes:get_dexterity(Attributes)},
         {<<"int">>, shr_attributes:get_intelligence(Attributes)},
         {<<"min">>, shr_attributes:get_mind(Attributes)},
         {<<"spe">>, shr_attributes:get_speed(Attributes)},
         {<<"str">>, shr_attributes:get_strength(Attributes)}
      ]
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate
   (
      non_neg_integer(),
      btl_character:type(),
      non_neg_integer()
   )
   -> {list(any())}.
generate (IX, Character, PlayerIX) ->
   Attributes = btl_character:get_attributes(Character),
   {ActiveWeapon, SecondaryWeapon} = btl_character:get_weapon_ids(Character),
   CharacterPlayerIX = btl_character:get_player_index(Character),
   Location = btl_character:get_location(Character),

   {
      [
         {<<"msg">>, <<"add_char">>},
         {<<"ix">>, IX},
         {<<"nam">>, btl_character:get_name(Character)},
         {<<"rnk">>, rank_to_string(btl_character:get_rank(Character))},
         {<<"ico">>, btl_character:get_icon(Character)},
         {<<"prt">>, btl_character:get_portrait(Character)},
         {
            <<"hea">>,
            btl_character:get_current_health(Character)
         },
         {<<"lc">>, btl_location:encode(Location)},
         {<<"pla">>, CharacterPlayerIX},
         {
            <<"ena">>,
            (
               btl_character:get_is_active(Character)
               and (CharacterPlayerIX == PlayerIX)
            )
         },
         {<<"dea">>, btl_character:get_is_defeated(Character)},
         {<<"att">>, attributes_as_json(Attributes)},
         {<<"awp">>, ActiveWeapon},
         {<<"swp">>, SecondaryWeapon},
         {<<"ar">>, btl_character:get_armor_id(Character)}
      ]
   }.
