; Create statements
[
  (create_table)
  (create_view)
  (create_materialized_view)
  (create_function)
  (create_index)
  (create_trigger)
  (create_type)
] @fold

; Blocks and expressions
[
  (block)
  (case)
  (select)
  (subquery)
  (function_body)
] @fold

; Other
[
  (comment)
] @fold
