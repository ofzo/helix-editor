; Type declarations
[
  (class_declaration)
  (interface_declaration)
  (enum_declaration)
  (annotation_type_declaration)
  (record_declaration)
] @fold

; Declaration bodies
[
  (class_body)
  (interface_body)
  (enum_body)
  (annotation_type_body)
  (constructor_body)
] @fold

; Method declarations
[
  (method_declaration)
  (constructor_declaration)
  (static_initializer)
  (lambda_expression)
] @fold

; Blocks
[
  (block)
  (switch_block)
] @fold

; Control flow
[
  (if_statement)
  (for_statement)
  (enhanced_for_statement)
  (while_statement)
  (do_statement)
] @fold

; Exception handling
[
  (try_statement)
  (try_with_resources_statement)
  (synchronized_statement)
] @fold

; Other
[
  (array_initializer)
  (block_comment)
] @fold
