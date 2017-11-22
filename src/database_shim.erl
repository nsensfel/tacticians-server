-module(database_shim).
-export
(
   [
      generate_db/1,
      fetch/2,
      commit/3
   ]
).

-include("timed_cache_data.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_db (_Heir) ->
   ets:new
   (
      db_shim,
      [
         set,
         public,
         named_table,
         {keypos, 1},
         {read_concurrency, true}
      ]
   ),
   io:format("~ndb_shim ets created.~n").

add_to_db (ID, Val) ->
   io:format("~nadd to db_shim: ~p.~n", [{ID, Val}]),
   ets:insert(db_shim, {ID, Val}).

generate_char_instances (Battlemap, Characters) ->
   lists:map
   (
      fun (Char) ->
         {
            Char#character.id,
            #character_instance
            {
               x = rand:uniform(Battlemap#battlemap.width - 1),
               y = rand:uniform(Battlemap#battlemap.height - 1),
               team = (rand:uniform(2) - 1)
            }
         }
      end,
      Characters
   ).

generate_map_instance (CharInts) ->
   #battlemap_instance
   {
      id = <<"0">>,
      chars = dict:from_list(CharInts),
      curr_player = 0,
      players = array:from_list([<<"0">>, <<"1">>]),
      rem_chars =
         lists:filtermap
         (
            fun ({K, V}) ->
               case character_instance:get_owner(V) of
                  0 -> {true, K};
                  _ -> false
               end
            end,
            CharInts
         ),
      last_turn = []
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_db (Heir) ->
   Pid = self(),
   spawn(fun () -> create_db(Heir), Pid ! ok, receive ok -> ok end end),
   receive
      ok -> ok
   end,
   Battlemap = battlemap_shim:generate(),
   Characters = character_shim:generate(rand:uniform(14) + 2),
   CharacterInsts = generate_char_instances(Battlemap, Characters),
   BattlemapInstance = generate_map_instance(CharacterInsts),
   add_to_db({battlemap_db, Battlemap#battlemap.id}, Battlemap),
   lists:map
   (
      fun (Char) ->
         add_to_db({character_db, Char#character.id}, Char)
      end,
      Characters
   ),
   add_to_db
   (
      {battlemap_instance_db, BattlemapInstance#battlemap_instance.id},
      BattlemapInstance
   ).

fetch (DB, ObjectID) ->
   io:format("~ndb_shim lookup: ~p.~n", [{DB, ObjectID}]),
   case ets:lookup(db_shim, {DB, ObjectID}) of
      [{_Key, Value}] -> {ok, Value};
      [] -> nothing
   end.

commit (DB, ObjectID, Value) ->
   add_to_db({DB, ObjectID}, Value),
   timed_cache:invalidate(DB, ObjectID).
