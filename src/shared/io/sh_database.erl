-module(sh_database).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      insert/4,
      fetch/2,
      commit/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_db_node () -> list_to_atom("db_node@" ++ net_adm:localhost()).

do_remote_operation (Op, Params) ->
   rpc:call(get_db_node(), db_access, Op, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec insert (atom(), any(), sh_db_user:permission(), any()) -> 'ok'.
insert (DB, ObjectID, Permission, Value) ->
   {atomic, _} = do_remote_operation(insert, [DB, ObjectID, Permission, Value]),
   io:format
   (
      "~nsh_database:insert(~p) -> ok.~n",
      [{DB, ObjectID, Permission, Value}]
   ),

   ok.

-spec fetch (atom(), any()) -> ({'ok', any()} | 'not_found').
fetch (DB, ObjectID) ->
   {atomic, Reply} = do_remote_operation(read, [DB, ObjectID]),
   io:format("~nsh_database:fetch(~p) -> ~p.~n", [{DB, ObjectID}, Reply]),
   Reply.

-spec commit (sh_db_query:type()) -> 'ok'.
commit (Query) ->
   {atomic, ok} = do_remote_operation(query, [Query]),
   io:format("~nsh_database:commit(~p) -> ok.~n", [Query]),
   ok.
