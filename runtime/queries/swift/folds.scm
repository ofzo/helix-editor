; Declarations
[
  (function_declaration)
  (function_body)
  (class_declaration)
  (class_body)
  (protocol_declaration)
  (protocol_body)
  (enum_class_body)
  (init_declaration)
] @fold

; Control flow
[
  (if_statement)
  (for_statement)
  (while_statement)
  (repeat_while_statement)
  (guard_statement)
] @fold

; Switch and error handling
[
  (switch_statement)
  (switch_entry)
  (do_statement)
  (catch_block)
] @fold

; Expressions and literals
[
  (lambda_literal)
  (computed_property)
  (array_literal)
  (dictionary_literal)
] @fold

; Other
[
  (comment)
] @fold
