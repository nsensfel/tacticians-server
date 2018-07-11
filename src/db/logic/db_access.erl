-module(db_access).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      read/3,
      insert/5,
      query/1
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec query_transaction (shr_db_query:type()) -> 'ok'.
query_transaction (Query) ->
   DB = shr_db_query:get_database(Query),
   ID = shr_db_query:get_entry_id(Query),
   [Item] = mnesia:read(DB, ID),
   {ok, UpdatedItem} = shr_db_query:apply_to(Query, Item),
   % FIXME: handle return value, mnesia:write -> (transaction abort | ok).
   % FIXME: is this an atomic OP? Is the lock freed afterwards?
   mnesia:write(DB, UpdatedItem, sticky_write),
   ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec read
   (
      atom(),
      any(),
      shr_db_user:user()
   )
   -> ({'aborted', any()} | {'atomic', ({'ok', any()} | 'not_found')}).
read (DB, ID, Cred) ->
   case mnesia:transaction(fun mnesia:read/2, [DB, ID]) of
      {'atomic', []} -> {'atomic', 'not_found'};
      {'atomic', [Item]} ->
         true =
            shr_db_user:can_access
            (
               shr_db_item:get_write_permission(Item),
               Cred
            ),
         {'atomic', {ok, shr_db_item:get_value(Item)}};

      Other -> {'aborted', Other}
   end.

-spec insert
   (
      atom(),
      any(),
      shr_db_user:permission(),
      shr_db_user:permission(),
      any())
   -> ({'aborted', any()} | {'atomic', 'ok'}).
insert (DB, ID, ReadPerm, WritePerm, Value) ->
   Item = shr_db_item:new(ID, ReadPerm, WritePerm, Value),
   mnesia:transaction(fun mnesia:write/3, [DB, Item, sticky_write]).

-spec query (shr_db_query:type()) -> ({'aborted', any()} | {'atomic', 'ok'}).
query (Query) ->
   mnesia:transaction(fun query_transaction/1, [Query]).
