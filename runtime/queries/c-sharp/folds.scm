; Type declarations
[
  (class_declaration)
  (struct_declaration)
  (interface_declaration)
  (enum_declaration)
  (record_declaration)
] @fold

; Namespace declarations
[
  (namespace_declaration)
  (file_scoped_namespace_declaration)
] @fold

; Member declarations
[
  (method_declaration)
  (constructor_declaration)
  (destructor_declaration)
  (operator_declaration)
  (conversion_operator_declaration)
] @fold

; Property and event declarations
[
  (property_declaration)
  (indexer_declaration)
  (event_declaration)
  (accessor_list)
] @fold

; Blocks
[
  (block)
  (declaration_list)
  (enum_member_declaration_list)
  (switch_body)
] @fold

; Control flow
[
  (if_statement)
  (for_statement)
  (foreach_statement)
  (while_statement)
  (do_statement)
] @fold

; Statement blocks
[
  (try_statement)
  (checked_statement)
  (unsafe_statement)
  (lock_statement)
  (using_statement)
  (fixed_statement)
] @fold

; Lambdas
[
  (lambda_expression)
  (anonymous_method_expression)
] @fold

; Other
[
  (comment)
] @fold
