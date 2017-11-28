-module(character_shim).
-record
(
   character,
   {
      id,
      name,
      icon,
      portrait,
      health,
      mov_pts,
      atk_rg
   }
).
-export
(
   [
      generate_random/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_char (N) ->
   IDAsString = list_to_binary(integer_to_list(N)),
   #character
   {
      id = IDAsString, % ID
      name = IDAsString, % Name
      icon = IDAsString, % Icon
      portrait = IDAsString, % Portrait
      health = (rand:uniform(5) + 1),
      mov_pts = (rand:uniform(10) + 10), % Movement Points
      atk_rg = (rand:uniform(5) - 1) % Attack Range
   }.

generate (0, Result) ->
   Result;
generate (N, Prev) ->
   generate((N - 1), [generate_char(N - 1)|Prev]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_random (N) ->
   generate(N, []).
