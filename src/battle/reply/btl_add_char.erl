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
   {ActiveWeapon, SecondaryWeapon} = btl_character:get_weapon_ids(Character),
   CharacterPlayerIX = btl_character:get_player_index(Character),
   Location = btl_character:get_location(Character),

   {
      [
         {<<"msg">>, <<"add_char">>},
         {<<"ix">>, IX},
         {<<"nam">>, btl_character:get_name(Character)},
         {<<"rnk">>, rank_to_string(btl_character:get_rank(Character))},
         {<<"prt">>, btl_character:get_portrait_id(Character)},
         {
            <<"hea">>,
            btl_character:get_current_health(Character)
         },
         {<<"lc">>, shr_location:encode(Location)},
         {<<"pla">>, CharacterPlayerIX},
         {
            <<"ena">>,
            (
               btl_character:get_is_active(Character)
               and (CharacterPlayerIX == PlayerIX)
            )
         },
         {<<"dea">>, btl_character:get_is_defeated(Character)},
         {<<"awp">>, ActiveWeapon},
         {<<"swp">>, SecondaryWeapon},
         {<<"ar">>, btl_character:get_armor_id(Character)},
         {
            <<"pomni">>,
            shr_omnimods:encode(btl_character:get_permanent_omnimods(Character))
         }
      ]
   }.
