-module(battlemap).
-export
(
   [
      cross/4
   ]
).

-include("timed_cache_data.hrl").

calc_new_loc (X, Y, [], _Points, _Map, _OtherChars) ->
   {X, Y};
calc_new_loc (X, Y, [Step|Path], Points, Map, OtherChars) ->
   case Step of
      <<"U">> -> calc_new_loc(X, (Y - 1), Path, Points, Map, OtherChars);
      <<"D">> -> calc_new_loc(X, (Y + 1), Path, Points, Map, OtherChars);
      <<"L">> -> calc_new_loc((X - 1), Y, Path, Points, Map, OtherChars);
      <<"R">> -> calc_new_loc((X + 1), Y, Path, Points, Map, OtherChars);
      _ -> calc_new_loc(X, Y, Path, Points, Map, OtherChars)
   end.

cross (Battlemap, CharInst, Path, OtherChars) ->
   {X, Y} = character_instance:get_location(CharInst),
   {ok, calc_new_loc(X, Y, Path, 99, Battlemap, OtherChars)}.
