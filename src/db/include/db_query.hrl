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
      perm :: db_user:permission()
   }
).

-record
(
   db_query,
   {
      db :: atom(),
      id :: any(),
      user :: db_user:user(),
      ops :: list(db_query_master_op())
   }
).

-type db_query_op() :: (#set_field{} | #add_to_field{} | #update_indexed{}).
-type db_query_master_op() :: (db_query_op() | #set_perm{}).
-type db_query() :: #db_query{}.

