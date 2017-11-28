-module(battlemap_instance_shim).
-record
(
   battlemap_instance,
   {
      id,
      chars,
      curr_player,
      players,
      rem_chars,
      last_turn
   }
).
-export
(
   [
      generate_random/2
   ]
).

generate_random (CharInsts, Players) ->
   #battlemap_instance
   {
      id = <<"0">>,
      chars = dict:from_list(CharInsts),
      curr_player = 0,
      players = array:from_list(Players),
      rem_chars =
         lists:filtermap
         (
            fun ({K, V}) ->
               case character_instance:get_owner(V) of
                  0 -> {true, K};
                  _ -> false
               end
            end,
            CharInsts
         ),
      last_turn = []
   }.
