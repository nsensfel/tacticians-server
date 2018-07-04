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
-spec rank_to_string (bm_character:rank()) -> binary().
rank_to_string (Rank) ->
   case Rank of
      optional -> <<"o">>;
      target -> <<"t">>;
      commander -> <<"c">>
   end.

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
      non_neg_integer()
   )
   -> {list(any())}.
generate (IX, Character, PlayerIX) ->
   Attributes = bm_character:get_attributes(Character),
   {ActiveWeapon, SecondaryWeapon} = bm_character:get_weapon_ids(Character),
   CharacterPlayerIX = bm_character:get_player_index(Character),
   Location = bm_character:get_location(Character),

   {
      [
         {<<"msg">>, <<"add_char">>},
         {<<"ix">>, IX},
         {<<"nam">>, bm_character:get_name(Character)},
         {<<"rnk">>, rank_to_string(bm_character:get_rank(Character))},
         {<<"ico">>, bm_character:get_icon(Character)},
         {<<"prt">>, bm_character:get_portrait(Character)},
         {
            <<"hea">>,
            bm_character:get_current_health(Character)
         },
         {<<"lc">>, bm_location:encode(Location)},
         {<<"pla">>, CharacterPlayerIX},
         {
            <<"ena">>,
            (
               bm_character:get_is_active(Character)
               and (CharacterPlayerIX == PlayerIX)
            )
         },
         {<<"dea">>, bm_character:get_is_defeated(Character)},
         {<<"att">>, attributes_as_json(Attributes)},
         {<<"awp">>, ActiveWeapon},
         {<<"swp">>, SecondaryWeapon},
         {<<"ar">>, bm_character:get_armor_id(Character)}
      ]
   }.
