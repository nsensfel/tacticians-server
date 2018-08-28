-module(btl_shim).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate_random_battle/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_random_characters
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      btl_map:type(),
      list(btl_location:type()),
      list(btl_character:type())
   )
   -> list(btl_character:type()).
generate_random_characters
(
   0,
   0,
   _CharactersPerPlayer,
   _TotalCharacterCount,
   _Map,
   _ForbiddenLocations,
   Result
) ->
   Result;
generate_random_characters
(
   MaxPlayerIX,
   0,
   CharactersPerPlayer,
   TotalCharacterCount,
   Map,
   ForbiddenLocations,
   Result
) ->
   generate_random_characters
   (
      (MaxPlayerIX - 1),
      CharactersPerPlayer,
      CharactersPerPlayer,
      TotalCharacterCount,
      Map,
      ForbiddenLocations,
      Result
   );
generate_random_characters
(
   MaxPlayerIX,
   PlayerCharacterCount,
   CharactersPerPlayer,
   TotalCharacterCount,
   Map,
   ForbiddenLocations,
   Result
) ->
   NewCharacter =
      btl_character:random
      (
         TotalCharacterCount,
         MaxPlayerIX,
         btl_map:get_width(Map),
         btl_map:get_height(Map),
         ForbiddenLocations
      ),
   Character =
      case MaxPlayerIX of
         0 -> btl_character:set_is_active(true, NewCharacter);
         _ -> NewCharacter
      end,

   generate_random_characters
   (
      MaxPlayerIX,
      (PlayerCharacterCount - 1),
      CharactersPerPlayer,
      (TotalCharacterCount + 1),
      Map,
      [btl_character:get_location(Character)|ForbiddenLocations],
      [Character|Result]
   ).

-spec new_demo_map () -> list(list(non_neg_integer())).
new_demo_map () ->
   [[4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [1,0,4,34], [1,0,4,36], [4,0], [1,0,4,18], [4,0],
   [4,0], [2,0,4,34], [2,0,4,36], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,16], [1,0,4,45],
   [1,0,4,40], [1,0,4,41], [1,0,2,12,4,40], [1,0,2,5,4,40], [2,0,4,43],
   [2,0,4,45], [2,0,4,36], [4,0], [2,0,4,34], [2,0,4,40], [2,0,4,40],
   [2,0,4,40], [1,0,2,3,4,36], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [1,0,4,18], [4,0], [4,0], [4,0], [4,0], [4,0],
   [1,0,4,34], [1,0,4,43], [1,0], [1,0], [1,0,2,12], [1,0,2,5], [2,0], [2,0],
   [2,0], [2,0,4,45], [2,0,4,40], [2,0,4,43], [2,0], [2,0], [2,0], [2,0,4,45],
   [2,0,4,40], [1,0,2,16,4,40], [1,0,4,40], [1,0,4,40], [1,0,4,36], [4,0],
   [4,0], [4,0], [1,0,4,34], [1,0,4,40], [1,0,4,29], [4,0], [4,0], [4,0],
   [4,0], [1,0,4,34], [1,0,4,43], [1,0], [1,0], [1,0,2,12], [1,0,2,5], [2,0],
   [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0],
   [1,0,2,34], [1,0,2,43], [1,0], [1,0], [1,0,4,45], [1,0,4,40],
   [1,0,2,12,4,40], [1,0,2,9,4,40], [1,0,2,9,4,43], [1,0,2,9], [1,0,2,10,4,45],
   [1,0,2,9,4,39], [1,0,2,11,4,21], [4,0], [4,0], [1,0,4,3], [1,0,4,11], [1,0],
   [1,0,2,12], [1,0,2,5], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0],
   [2,0], [2,0], [2,0], [3,0,2,34], [1,0,2,40], [1,0,2,43], [1,0], [1,0],
   [1,0], [1,0], [1,0,2,12], [1,0,2,5], [2,0], [2,0], [2,0], [1,0,2,23],
   [2,0,4,30], [4,0], [4,0], [4,0], [4,0], [1,0,4,3], [1,0,4,11], [1,0,2,30],
   [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0],
   [3,0,2,34], [3,0,2,43], [1,0], [1,0], [1,0], [1,0], [1,0], [1,0],
   [1,0,2,30], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0,4,30], [4,0], [4,0],
   [4,0], [4,0], [4,0], [1,0,4,16], [1,0,2,30], [2,0], [2,0], [2,0], [2,0],
   [2,0], [2,0], [2,0], [2,0], [3,0,2,34], [3,0,2,40], [3,0,2,43], [3,0],
   [3,0], [1,0], [1,0], [1,0], [1,0], [1,0], [1,0,2,31], [1,0,2,21], [2,0],
   [2,0], [2,0], [2,0], [2,0,4,30], [4,0], [4,0], [4,0], [4,0], [4,0],
   [1,0,4,16], [1,0,2,45], [1,0,2,36], [2,0], [2,0], [2,0], [2,0], [2,0],
   [2,0], [3,0,2,34], [3,0,2,43], [3,0], [3,0], [3,0], [1,0], [1,0], [1,0],
   [1,0], [1,0], [1,0], [1,0,2,30], [2,0], [2,0], [2,0], [2,0], [2,0,4,12],
   [1,0,2,34,4,8], [1,0,2,43,4,21], [4,0], [4,0], [4,0], [1,0,4,34],
   [1,0,4,43], [1,0], [1,0,2,45], [1,0,2,36], [2,0], [2,0], [2,0], [2,0],
   [3,0,2,34], [3,0,2,43], [3,0], [3,0], [3,0], [3,0], [1,0], [1,0], [1,0],
   [1,0], [1,0,4,12], [1,0,4,9], [1,0,2,30,4,11], [2,0], [1,0,2,34],
   [1,0,2,40,4,12], [1,0,2,40,4,9], [1,0,2,40,4,27], [4,0], [4,0], [4,0],
   [4,0], [4,0], [1,0,4,3], [1,0,4,11], [1,0], [1,0], [1,0,2,45], [1,0,2,36],
   [2,0], [2,0], [3,0,2,34], [3,0,2,43], [3,0], [3,0], [3,0], [3,0], [1,0],
   [1,0], [1,0], [1,0,4,12], [1,0,4,9], [1,0,4,5], [4,0], [1,0,2,45,4,3],
   [1,0,2,40,4,9], [1,0,2,43,4,9], [1,0,4,5], [4,0], [1,0,4,23], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [1,0,4,16], [1,0], [1,0], [1,0], [1,0,2,45],
   [1,0,2,40], [1,0,2,40], [3,0,2,43], [3,0], [3,0], [3,0], [1,0], [1,0],
   [1,0], [1,0], [1,0], [1,0,4,30], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,34],
   [1,0,4,43], [1,0], [1,0], [1,0], [1,0], [1,0,2,12], [1,0,2,9], [1,0,2,11],
   [1,0], [1,0], [3,0,2,12], [1,0,2,9], [1,0,2,11], [1,0], [1,0], [1,0],
   [1,0,4,45], [1,0,4,40], [1,0,4,36], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,3], [1,0,4,11],
   [1,0], [1,0], [1,0], [1,0], [1,0,2,30], [2,0], [1,0,2,3], [1,0,2,10],
   [1,0,2,9], [1,0,2,5], [2,0], [1,0,2,3], [1,0,2,11], [1,0], [1,0], [1,0],
   [1,0], [1,0,4,45], [1,0,4,40], [1,0,4,40], [1,0,4,36], [4,0], [2,0,4,34],
   [2,0,4,40], [2,0,4,36], [4,0], [1,0,4,18], [4,0], [4,0], [4,0], [4,0],
   [1,0,4,16], [1,0,2,12], [1,0,2,9], [1,0,2,9], [1,0,2,9], [1,0,2,5], [2,0],
   [2,0], [1,0,2,23], [2,0], [2,0], [2,0], [2,0], [1,0,2,16], [1,0], [1,0],
   [1,0], [1,0], [1,0], [1,0], [1,0], [1,0,2,12,4,45], [1,0,2,5,4,40],
   [2,0,4,43], [2,0], [2,0,4,45], [1,0,2,16,4,39], [1,0,4,4], [4,0], [4,0],
   [4,0], [4,0], [1,0,4,3], [1,0,2,30,4,11], [2,0], [2,0], [2,0], [2,0], [2,0],
   [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [1,0,2,16], [1,0], [3,0],
   [1,0,4,12], [1,0,4,9], [1,0,4,11], [1,0], [1,0], [1,0,2,30], [2,0], [2,0],
   [2,0], [1,0,2,34], [1,0,2,43,4,30], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [1,0,2,30,4,16], [2,0], [2,0], [2,0], [1,0,2,34], [1,0,2,36], [2,0],
   [1,0,2,18], [2,0], [2,0], [2,0], [1,0,2,34], [1,0,2,43], [3,0], [3,0,4,12],
   [1,0,4,5], [4,0], [1,0,4,3], [1,0,4,9], [1,0,4,11], [1,0,2,30], [2,0],
   [2,0], [2,0], [1,0,2,3], [1,0,2,9,4,30], [4,0], [4,0], [4,0], [4,0], [4,0],
   [1,0,4,34], [1,0,2,45,4,43], [1,0,2,36], [2,0], [1,0,2,34], [1,0,2,43],
   [1,0,2,45], [1,0,2,40], [1,0,2,29], [2,0], [3,0,2,18], [2,0], [3,0,2,16],
   [3,0], [3,0], [3,0,4,30], [4,0], [4,0], [4,0], [4,0], [1,0,4,16],
   [1,0,2,30], [2,0], [2,0], [2,0], [2,0], [2,0,4,31], [2,0,4,21], [4,0],
   [4,0], [4,0], [4,0], [1,0,4,16], [1,0], [1,0,2,45], [1,0,2,40], [1,0,2,43],
   [1,0,4,12], [1,0,4,9], [1,0,4,9], [1,0,2,45,4,11], [1,0,2,40], [3,0,2,41],
   [3,0,2,40], [3,0,2,43], [3,0], [3,0], [3,0,4,45], [1,0,4,36], [4,0], [4,0],
   [1,0,4,34], [1,0,4,43], [1,0,2,45], [1,0,2,36], [2,0], [2,0], [2,0,4,12],
   [2,0,4,5], [4,0], [4,0], [4,0], [4,0], [1,0,4,22], [1,0,4,17], [1,0], [1,0],
   [1,0,4,12], [1,0,4,9], [1,0,4,5], [4,0], [4,0], [1,0,4,16], [3,0], [3,0],
   [3,0], [3,0], [3,0], [3,0], [3,0,2,12], [1,0,2,9,4,45], [1,0,2,9,4,40],
   [1,0,2,9,4,40], [1,0,2,11,4,43], [1,0], [1,0], [1,0,2,45], [1,0,2,36],
   [2,0,4,12], [2,0,4,5], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,16],
   [1,0], [1,0], [1,0,4,30], [4,0], [4,0], [4,0], [4,0], [1,0,4,16], [3,0],
   [3,0], [3,0], [3,0,2,12], [3,0,2,9], [3,0,2,9], [3,0,2,5], [2,0], [2,0],
   [2,0], [1,0,2,16], [1,0], [1,0], [1,0], [1,0,2,45], [1,0,2,40,4,30], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,16], [1,0], [1,0],
   [1,0,4,30], [4,0], [4,0], [4,0], [4,0], [1,0,4,16], [1,0], [3,0,2,12],
   [3,0,2,9], [3,0,2,5], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0], [1,0,2,16],
   [1,0], [1,0], [1,0], [1,0], [1,0,4,31], [1,0,4,21], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [1,0,4,16], [1,0], [1,0], [1,0,4,30], [4,0], [4,0],
   [4,0], [4,0], [1,0,4,16], [1,0], [1,0,2,30], [2,0], [2,0], [2,0], [2,0],
   [2,0], [2,0], [2,0], [1,0,2,34], [1,0,2,43], [1,0], [1,0], [1,0],
   [1,0,4,12], [1,0,4,5], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [1,0,4,16], [1,0], [1,0], [1,0,4,30], [4,0], [4,0], [1,0,4,34], [1,0,4,40],
   [1,0,4,43], [1,0], [1,0,2,30], [2,0], [2,0], [2,0], [2,0], [2,0],
   [1,0,2,34], [1,0,2,40], [1,0,2,43], [1,0], [1,0], [1,0], [1,0], [1,0,4,30],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,16], [1,0],
   [1,0], [1,0,4,45], [1,0,4,40], [1,0,4,40], [1,0,2,12,4,43], [1,0,2,9],
   [1,0,2,9], [1,0,2,9], [1,0,2,44], [1,0,2,40], [1,0,2,40], [1,0,2,40],
   [1,0,2,40], [1,0,2,40], [1,0,2,43], [1,0], [1,0], [1,0], [1,0], [1,0],
   [1,0,4,12], [1,0,4,5], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [1,0,4,34], [1,0,4,43], [1,0], [1,0], [1,0,2,12], [1,0,2,9], [1,0,2,9],
   [1,0,2,5], [2,0], [2,0], [2,0], [1,0,2,3], [1,0,2,11], [1,0], [1,0], [1,0],
   [1,0], [1,0], [1,0], [1,0], [1,0], [1,0], [1,0,4,12], [1,0,4,5], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,33], [1,0,4,7], [1,0,4,9],
   [1,0,4,11], [1,0], [1,0,2,30], [2,0], [2,0], [2,0], [2,0], [2,0], [2,0],
   [2,0], [1,0,2,16], [1,0], [1,0], [1,0], [1,0], [1,0], [1,0], [1,0], [1,0],
   [1,0], [1,0,4,30], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [1,0,4,23], [4,0], [4,0], [1,0,4,16], [1,0], [1,0,2,45], [1,0,2,40],
   [1,0,2,36], [2,0], [2,0], [2,0], [2,0], [2,0], [1,0,2,16], [1,0],
   [1,0,4,12], [1,0,4,9], [1,0,4,11], [1,0], [1,0,4,12], [1,0,4,9], [1,0,4,9],
   [1,0,4,10], [1,0,4,8], [1,0,4,21], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [1,0,4,16], [1,0,4,12], [1,0,4,9], [1,0,4,9],
   [1,0,2,45,4,9], [1,0,2,40,4,11], [1,0,2,36], [2,0], [2,0], [1,0,2,34,4,12],
   [1,0,2,43,4,9], [1,0,4,9], [1,0,4,5], [4,0], [1,0,4,3], [1,0,4,9],
   [1,0,4,5], [4,0], [4,0], [1,0,4,26], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,22], [1,0,4,17],
   [1,0,4,30], [4,0], [4,0], [4,0], [1,0,4,16], [1,0,2,45,4,12],
   [1,0,2,40,4,9], [1,0,2,40,4,10], [1,0,2,43,4,5], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,23], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [1,0,4,3],
   [1,0,4,5], [4,0], [4,0], [1,0,4,22], [1,0,4,7], [1,0,4,5], [4,0],
   [1,0,4,23], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0],
   [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0], [4,0]].

-spec demo_map () -> list(list(non_neg_integer())).
demo_map () ->
   [
      [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0], [2, 0],
      [2, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [2, 0], [2, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [2, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [2, 0], [3, 0], [3, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [2, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [3, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [2, 0], [1, 0], [2, 0],
      [2, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0], [2, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [3, 0], [2, 0], [1, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [3, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [2, 0],
      [2, 0], [3, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [1, 0], [2, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [2, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [2, 0],
      [2, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [2, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [2, 0],
      [2, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [3, 0], [3, 0], [1, 0], [3, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [2, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [2, 0],
      [2, 0], [3, 0], [3, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [1, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [2, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [3, 0], [3, 0], [2, 0],
      [2, 0], [3, 0], [3, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [1, 0], [3, 0], [0, 0], [0, 0], [0, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [1, 0], [1, 0], [3, 0], [1, 0], [3, 0], [3, 0], [3, 0], [2, 0],
      [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0],
      [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0], [0, 0]
   ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_random_battle () -> btl_battle:type().
generate_random_battle () ->
   %MapWidth = 32, % shr_roll:between(16, 32),
   %MapHeight = 32, %shr_roll:between(16, 32),
   %Map = btl_map:random(0, MapWidth, MapHeight),
   Map = btl_map:from_list(0, 32, 32, new_demo_map()),
   Characters = generate_random_characters(1, 8, 8, 0, Map, [], []),
   PlayersAsList = [btl_player:new(0, 8, <<"0">>), btl_player:new(1, 0, <<"1">>)],

   {UsedWeaponIDs, UsedArmorIDs} =
      lists:foldl
      (
         fun (Character, {UWIDs, UAIDs}) ->
            {MWpID, SWpID} = btl_character:get_weapon_ids(Character),
            AID = btl_character:get_armor_id(Character),
            {
               sets:add_element(MWpID, sets:add_element(SWpID, UWIDs)),
               sets:add_element(AID, UAIDs)
            }
         end,
         {sets:new(), sets:new()},
         Characters
      ),

   UsedTileIDs =
      array:sparse_foldl
      (
         fun (_IX, TileInstance, CurrentTileIDs) ->
            sets:add_element
            (
               shr_tile:extract_main_class_id(TileInstance),
               CurrentTileIDs
            )
         end,
         sets:new(),
         btl_map:get_tile_instances(Map)
      ),

   Battle =
      btl_battle:new
      (
         <<"0">>,
         PlayersAsList,
         Map,
         Characters,
         sets:to_list(UsedWeaponIDs),
         sets:to_list(UsedArmorIDs),
         sets:to_list(UsedTileIDs)
      ),

   Battle.
