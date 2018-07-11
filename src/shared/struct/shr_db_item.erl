-module(shr_db_item).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   db_item,
   {
      id :: any(),
      read_perm :: shr_db_user:permission(),
      write_perm :: shr_db_user:permission(),
      val :: any()
   }
).

-type db_item() :: #db_item{}.

-type type() :: db_item().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      new/4,

      get_id/1,
      get_read_permission/1,
      get_write_permission/1,
      get_value/1,

      set_read_permission/2,
      set_write_permission/2,
      set_value/2,

      get_id_field/0,
      get_record_info/0,
      get_record_name/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new
   (
      any(),
      shr_db_user:permission(),
      shr_db_user:permission(),
      any()
   ) -> type().
new (ID, ReadPermission, WritePermission, Value) ->
   #db_item
   {
      id = ID,
      read_perm = ReadPermission,
      write_perm = WritePermission,
      val = Value
   }.

-spec get_id (type()) -> any().
get_id (#db_item { id = Result }) -> Result.

-spec get_read_permission (type()) -> shr_db_user:permission().
get_read_permission (#db_item { read_perm = Result }) -> Result.

-spec get_write_permission (type()) -> shr_db_user:permission().
get_write_permission (#db_item { write_perm = Result }) -> Result.

-spec get_value (type()) -> any().
get_value (#db_item { val = Result }) -> Result.

-spec set_read_permission (shr_db_user:permission(), type()) -> type().
set_read_permission (Perm, Item) -> Item#db_item{ read_perm = Perm }.

-spec set_write_permission (shr_db_user:permission(), type()) -> type().
set_write_permission (Perm, Item) -> Item#db_item{ write_perm = Perm }.

-spec set_value (any(), type()) -> type().
set_value (Value, Item) -> Item#db_item{ val = Value }.

-spec get_id_field () -> non_neg_integer().
get_id_field () -> #db_item.id.

get_record_info () -> record_info(fields, db_item).

get_record_name () -> db_item.

