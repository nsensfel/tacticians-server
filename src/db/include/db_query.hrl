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
      update :: list(db_query())
   }
).

-type db_query() :: (#set_field{} | #add_to_field{} | #update_indexed{}).

