-module(shr_db_query).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   set_field,
   {
      field :: non_neg_integer(),
      value :: any()
   }
).

-record
(
   add_to_field,
   {
      field :: non_neg_integer(),
      values :: list(any()),
      head :: boolean()
   }
).

-record
(
   update_indexed,
   {
      field :: non_neg_integer(),
      ix :: non_neg_integer(),
      ops :: list(db_query_op())
   }
).

-record
(
   set_perm,
   {
      perm :: shr_db_user:permission()
   }
).

-record
(
   set_val,
   {
      val :: any()
   }
).

-record
(
   db_query,
   {
      db :: atom(),
      id :: any(),
      user :: shr_db_user:user(),
      ops :: list(db_query_master_op())
   }
).

-type db_query_op() :: (#set_field{} | #add_to_field{} | #update_indexed{}).
-type db_query_master_op() :: (db_query_op() | #set_perm{} | #set_val{}).
-type db_query() :: #db_query{}.

-opaque op() :: db_query_op().
-opaque type() :: db_query().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0, op/0]).

-export
(
   [
      new/4,
      set_field/2,
      add_to_field/3,
      update_indexed/3
   ]
).
-export
(
   [
      get_database/1,
      get_entry_id/1
   ]
).
-export([apply_to/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_user (db_query()) -> shr_db_user:user().
get_user (#db_query{ user = Result }) -> Result.

-spec apply_update_indexed (#update_indexed{}, any()) -> any().
apply_update_indexed (Op, Elem) ->
   FieldNumber = Op#update_indexed.field,
   IX = Op#update_indexed.ix,
   Ops = Op#update_indexed.ops,

   IndexedFieldValue = element(FieldNumber, Elem),
   ArrayValue = array:get(IX, IndexedFieldValue),
   UpdatedArrayValue = lists:foldl(fun apply_op_to/2, ArrayValue, Ops),
   UpdatedIndexedFieldValue =
      array:set(IX, UpdatedArrayValue, IndexedFieldValue),

   setelement(FieldNumber, Elem, UpdatedIndexedFieldValue).

-spec apply_add_to_field (#add_to_field{}, any()) -> any().
apply_add_to_field (Op, Elem) ->
   FieldNumber = Op#add_to_field.field,
   NewValues = Op#add_to_field.values,
   AddToHead = Op#add_to_field.head,

   CurrentValues = element(FieldNumber, Elem),
   UpdatedValues =
      case AddToHead of
         true -> (NewValues ++ CurrentValues);
         _ -> (CurrentValues ++ NewValues)
      end,

   setelement(FieldNumber, Elem, UpdatedValues).

-spec apply_set_field (#set_field{}, any()) -> any().
apply_set_field (Op, Elem) ->
   FieldNumber = Op#set_field.field,
   NewValue = Op#set_field.value,

   setelement(FieldNumber, Elem, NewValue).

-spec apply_op_to (db_query_op(), any()) -> any().
apply_op_to (Op, Elem) when is_record(Op, set_field) ->
   apply_set_field(Op, Elem);
apply_op_to (Op, Elem) when is_record(Op, add_to_field) ->
   apply_add_to_field(Op, Elem);
apply_op_to (Op, Elem) when is_record(Op, update_indexed) ->
   apply_update_indexed(Op, Elem).

-spec apply_master_op_to
   (
      db_query_master_op(),
      shr_db_item:type()
   )
   -> shr_db_item:type().
apply_master_op_to (MOp, Elem) when is_record(MOp, set_perm) ->
   NewPerm = MOp#set_perm.perm,

   shr_db_item:set_permission(NewPerm, Elem);
apply_master_op_to (MOp, Elem) when is_record(MOp, set_val) ->
   NewVal = MOp#set_val.val,

   shr_db_item:set_value(NewVal, Elem);
apply_master_op_to (MOp, Elem) ->
   OldValue = shr_db_item:get_value(Elem),
   NewValue = apply_op_to(MOp, OldValue),

   shr_db_item:set_value(NewValue, Elem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (atom(), any(), shr_db_user:user(), list(op())) -> type().
new (DBName, ObjectID, User, Ops) ->
   #db_query
   {
      db = DBName,
      id = ObjectID,
      user = User,
      ops = Ops
   }.

-spec set_field (non_neg_integer(), any()) -> op().
set_field (Field, Value) ->
   #set_field { field = Field, value = Value }.

-spec add_to_field (non_neg_integer(), list(any()), boolean()) -> op().
add_to_field (Field, Values, IsPrefix) ->
   #add_to_field { field = Field, values = Values, head = IsPrefix}.

-spec update_indexed
   (
      non_neg_integer(),
      non_neg_integer(),
      list(op())
   )
   -> op().
update_indexed (Field, IX, Updates) ->
   #update_indexed { field = Field, ix = IX, ops = Updates}.

-spec get_database (db_query()) -> atom().
get_database (#db_query{ db = Result }) -> Result.

-spec get_entry_id (db_query()) -> any().
get_entry_id (#db_query{ id = Result }) -> Result.

-spec apply_to
   (
      db_query(),
      shr_db_item:type()
   )
   -> ({'ok', shr_db_item:type()} | 'error').
apply_to (DBQuery, DBItem) ->
   true =
      shr_db_user:can_access
      (
         shr_db_item:get_permission(DBItem),
         get_user(DBQuery)
      ),
   MOps = DBQuery#db_query.ops,
   {ok, lists:foldl(fun apply_master_op_to/2, DBItem, MOps)}.

