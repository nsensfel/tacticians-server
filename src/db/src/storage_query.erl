-module(storage_query).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../include/db_query.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([apply_to/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_user (db_query()) -> db_user().
get_user (#db_query{ user = Result }) -> Result.

-spec apply_update_indexed (#update_indexed{}, any()) -> any().
apply_update_indexed (Op, Elem) ->
   FieldNumber = Op#update_indexed.field,
   IX = Op#update_indexed.ix,
   Ops = Op#update_indexed.ops,

   IndexedFieldValue = element(FieldNumber, Elem),
   ArrayValue = array:get(IX, IndexedFieldValue),
   UpdatedArrayValue = lists:foldl(fun apply_op_to/2, ArrayValue, Ops),
   UpdatedIndexedFieldValue = array:set(IX, UpdatedArrayValue),

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
apply_op_to (Op, Elem) when is_record(Elem, set_field) ->
   apply_set_field(Op, Elem);
apply_op_to (Op, Elem) when is_record(Elem, add_to_field) ->
   apply_add_to_field(Op, Elem);
apply_op_to (Op, Elem) when is_record(Elem, update_indexed) ->
   apply_update_indexed(Op, Elem).

-spec apply_master_op_to
   (
      db_query_master_op(),
      db_item:type()
   )
   -> db_item:type().
apply_master_op_to (MOp, Elem) when is_record(MOp, set_user) ->
   NewUser = MOp#set_user.user,

   db_item:set_user(NewUser, Elem);
apply_master_op_to (MOp, Elem) ->
   OldValue = db_item:get_value(Elem),
   NewValue = apply_op_to(MOp, OldValue),

   db_item:set_value(NewValue, Elem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_to
   (
      db_query(),
      db_item:type()
   )
   -> ({'ok', db_item:type()} | 'error').
apply_to (DBQuery, DBItem) ->
   true = db_user:has_permission(db_item:get_user(DBItem), get_user(DBQuery)),
   {ok, apply_master_op_to(DBQuery, DBItem)}.

