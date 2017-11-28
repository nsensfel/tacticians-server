-module(database_shim).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      generate_db/1,
      fetch/2,
      commit/3,
      assert_session_is_valid/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
            character:get_id(Char),
            character_instance:new_instance_of
            (
               Char,
               (rand:uniform(2) - 1), % team,
               {
                  rand:uniform(battlemap:get_width(Battlemap) - 1), % X
                  rand:uniform(battlemap:get_height(Battlemap) - 1)  % Y
               }
            )
         }
      end,
      Characters
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_db (Heir) ->
   Pid = self(),
   spawn(fun () -> create_db(Heir), Pid ! ok, receive ok -> ok end end),
   receive
      ok -> ok
   end,
   Players = [<<"0">>, <<"1">>],
   Battlemap = battlemap_shim:generate_random(),
   Characters = character_shim:generate_random(rand:uniform(12) + 4),
   CharacterInsts = generate_char_instances(Battlemap, Characters),
   BattlemapInstance =
      battlemap_instance_shim:generate_random
      (
         CharacterInsts,
         Players
      ),
   add_to_db({battlemap_db, battlemap:get_id(Battlemap)}, Battlemap),
   lists:map
   (
      fun (Char) ->
         add_to_db({character_db, character:get_id(Char)}, Char)
      end,
      Characters
   ),
   add_to_db
   (
      {battlemap_instance_db, battlemap_instance:get_id(BattlemapInstance)},
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

assert_session_is_valid (_PlayerID, _SessionToken) ->
   % Ask PlayerID's login server if SessionToken is correct.
   % If so, update last login time to prevent relogin within
   % (database_timeout * 2).
   % If not, crash.
   ok.
