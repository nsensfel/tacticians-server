-module(bm_add_weapon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec encode_range_type (sh_weapon:range_type()) -> binary().
encode_range_type (melee) -> <<"m">>;
encode_range_type (ranged) -> <<"r">>.

-spec encode_range_modifier (sh_weapon:range_modifier()) -> binary().
encode_range_modifier (long) -> <<"l">>;
encode_range_modifier (short) -> <<"s">>.

-spec encode_damage_type (sh_weapon:damage_type()) -> binary().
encode_damage_type (slash) -> <<"s">>;
encode_damage_type (pierce) -> <<"p">>;
encode_damage_type (blunt) -> <<"b">>.

-spec encode_damage_modifier (sh_weapon:damage_modifier()) -> binary().
encode_damage_modifier (heavy) -> <<"h">>;
encode_damage_modifier (light) -> <<"l">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate (sh_weapon:type()) -> {list(any())}.
generate (Weapon) ->
   {
      [
         {<<"msg">>, <<"add_weapon">>},
         {<<"id">>, sh_weapon:get_id(Weapon)},
         {<<"nam">>, sh_weapon:get_name(Weapon)},
         {<<"rt">>, encode_range_type(sh_weapon:get_range_type(Weapon))},
         {
            <<"rm">>,
            encode_range_modifier(sh_weapon:get_range_modifier(Weapon))
         },
         {<<"dt">>, encode_damage_type(sh_weapon:get_damage_type(Weapon))},
         {
            <<"dm">>,
            encode_damage_modifier(sh_weapon:get_damage_modifier(Weapon))
         },
         {<<"cf">>, sh_weapon:get_coefficient(Weapon)}
      ]
   }.
