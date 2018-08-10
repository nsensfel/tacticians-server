-module(shr_database).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      insert_at/5,
      insert/4,
      remove/3,
      fetch/3,
      reserve/3,
      commit/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_debug_db_node () -> node().
get_debug_db_node () -> list_to_atom("db_node@" ++ net_adm:localhost()).

-spec get_random_db_node () -> node().
get_random_db_node () ->
   get_debug_db_node().

-spec get_db_node_for (binary()) -> node().
get_db_node_for (_ObjectID) ->
   get_debug_db_node().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec insert_at
   (
      atom(),
      binary(),
      shr_db_user:permission(),
      shr_db_user:permission(),
      any()
   )
   -> 'ok'.
insert_at (DB, ObjectID, ReadPerm, WritePerm, Value) ->
   DBNode = get_db_node_for(ObjectID),

   {atomic, _} =
      rpc:call
      (
         DBNode,
         db_access,
         insert_at,
         [DB, ObjectID, ReadPerm, WritePerm, Value]
      ),

   io:format
   (
      "~nshr_database:insert_at(~p) ! ~p -> ok.~n",
      [{DB, ObjectID, ReadPerm, WritePerm, Value}, DBNode]
   ),

   ok.

-spec insert
   (
      atom(),
      shr_db_user:permission(),
      shr_db_user:permission(),
      any()
   )
   -> {'ok', binary()}.
insert (DB, ReadPerm, WritePerm, Value) ->
   DBNode = get_random_db_node(),

   {atomic, {ok, ID}} =
      rpc:call(DBNode, db_access, insert, [DB, ReadPerm, WritePerm, Value]),

   io:format
   (
      "~nshr_database:insert(~p) ! ~p -> ok.~n",
      [{DB, ReadPerm, WritePerm, Value}, DBNode]
   ),

   {ok, ID}.

-spec fetch
   (
      atom(),
      binary(),
      shr_db_user:user()
   )
   -> ({'ok', any()} | 'not_found').
fetch (DB, ObjectID, Cred) ->
   DBNode = get_db_node_for(ObjectID),

   {atomic, Reply} = rpc:call(DBNode, db_access, read, [DB, ObjectID, Cred]),

   io:format
   (
      "~nshr_database:fetch(~p) ! ~p -> ~p.~n",
      [{DB, ObjectID, Cred}, DBNode, Reply]
   ),

   Reply.

-spec commit (shr_db_query:type()) -> 'ok'.
commit (Query) ->
   DBNode = get_db_node_for(shr_db_query:get_entry_id(Query)),

   {atomic, ok} = rpc:call(DBNode, db_access, query, [Query]),

   io:format("~nshr_database:commit(~p) ! ~p -> ok.~n", [Query, DBNode]),

   ok.

-spec remove
   (
      atom(),
      binary(),
      shr_db_user:user()
   )
   -> ('ok' | 'not_found').
remove (DB, ObjectID, Cred) ->
   DBNode = get_db_node_for(ObjectID),

   {atomic, _} = rpc:call(DBNode, db_access, remove, [DB, ObjectID, Cred]),

   io:format
   (
      "~nshr_database:remove(~p) ! ~p -> ok.~n",
      [{DB, ObjectID, Cred}, DBNode]
   ),

   ok.

-spec reserve
   (
      atom(),
      binary(),
      shr_db_user:user()
   )
   -> ('ok' | 'not_found').
reserve (DB, ObjectID, Cred) ->
   DBNode = get_db_node_for(ObjectID),

   {atomic, _} = rpc:call(DBNode, db_access, reserve, [DB, ObjectID, Cred]),

   io:format
   (
      "~nshr_database:reserve(~p) ! ~p -> ok.~n",
      [{DB, ObjectID, Cred}, DBNode]
   ),

   ok.
