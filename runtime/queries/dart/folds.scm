; Type declarations
[
  (class_definition)
  (class_body)
  (enum_declaration)
  (enum_body)
  (extension_declaration)
  (extension_body)
  (mixin_declaration)
] @fold

; Function bodies
[
  (function_body)
  (function_expression_body)
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
  (while_statement)
  (do_statement)
  (try_statement)
  (switch_statement)
] @fold

; Literals
[
  (list_literal)
  (set_or_map_literal)
  (record_literal)
] @fold

; Other
[
  (comment)
] @fold
