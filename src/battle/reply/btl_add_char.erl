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
   CharacterPlayerIX = btl_character:get_player_index(Character),

   {
      [
         {<<"msg">>, <<"add_char">>},
         {<<"ix">>, IX},
         {<<"rnk">>, rank_to_string(btl_character:get_rank(Character))},
         {
            <<"hea">>,
            btl_character:get_current_health(Character)
         },
         {<<"lc">>, shr_location:encode(btl_character:get_location(Character))},
         {<<"pla">>, CharacterPlayerIX},
         {
            <<"ena">>,
            (
               btl_character:get_is_active(Character)
               and (CharacterPlayerIX == PlayerIX)
            )
         },
         {<<"dea">>, btl_character:get_is_defeated(Character)},
         {
            <<"bas">>,
            shr_character:encode(btl_character:get_base_character(Character))
         }
      ]
   }.
