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
      remove/3,
      reserve/3,
      insert_at/5,
      insert/4,
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

   mnesia:write(DB, UpdatedItem, sticky_write),

   ok.

-spec add_new_item (atom(), shr_db_item:type()) -> 'ok'.
add_new_item (DB, Item) ->
   ID = shr_db_item:get_id(Item),
   [] = mnesia:read(DB, ID),

   mnesia:write(DB, Item, sticky_write),

   ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec read
   (
      atom(),
      binary(),
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
               shr_db_item:get_read_permission(Item),
               Cred
            ),
         {'atomic', {ok, shr_db_item:get_value(Item)}};

      Other -> {'aborted', Other}
   end.

-spec insert_at
   (
      atom(),
      binary(),
      shr_db_user:permission(),
      shr_db_user:permission(),
      any())
   -> ({'aborted', any()} | {'atomic', 'ok'}).
insert_at (DB, ID, ReadPerm, WritePerm, Value) ->
   Item = shr_db_item:new(ID, ReadPerm, WritePerm, Value),
   mnesia:transaction(fun add_new_item/2, [DB, Item]).

-spec insert
   (
      atom(),
      shr_db_user:permission(),
      shr_db_user:permission(),
      any())
   -> ({'aborted', any()} | {'atomic', {'ok', binary()}}).
insert (DB, ReadPerm, WritePerm, Value) ->
   ID = <<"?">>, %% TODO [FUNCTION: db][HIGH]: gen new ID.
   case insert_at(DB, ID, ReadPerm, WritePerm, Value) of
      {'atomic', 'ok'} -> {'atomic', {'ok', ID}};
      {aborted, Val} -> {aborted, Val}
   end.

-spec query (shr_db_query:type()) -> ({'aborted', any()} | {'atomic', 'ok'}).
query (Query) ->
   mnesia:transaction(fun query_transaction/1, [Query]).

-spec reserve
   (
      atom(),
      binary(),
      shr_db_user:user()
   )
   -> ({'aborted', any()} | {'atomic', 'ok'}).
reserve (DB, ID, Cred) ->
   insert_at
   (
      DB,
      ID,
      [Cred],
      [Cred],
      {
         reserved,
         <<"?">> %% TODO [FUNCTION: db][LOW]: timestamp
      }
   ).

-spec remove
   (
      atom(),
      binary(),
      shr_db_user:user()
   )
   -> ({'aborted', any()} | {'atomic', ({'ok', any()} | 'not_found')}).
remove (_DB, _ID, _Cred) ->
   %% TODO [FUNCTION: db][MEDIUM]: unimplemented
   %% Don't forget to checkt that Cred has write access before removing the
   %% value.
   {'aborted', 'unimplemented'}.
