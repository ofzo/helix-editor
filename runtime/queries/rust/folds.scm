; Definitions
[
  (function_item)
  (impl_item)
  (struct_item)
  (enum_item)
  (union_item)
  (trait_item)
  (mod_item)
  (type_item)
  (const_item)
  (static_item)
  (foreign_mod_item)
  (macro_definition)
] @fold

; Control flow
[
  (match_expression)
  (if_expression)
  (while_expression)
  (for_expression)
  (loop_expression)
  (closure_expression)
  (block)
  (unsafe_block)
  (async_block)
] @fold

; Data structures
[
  (array_expression)
  (tuple_expression)
  (struct_expression)
  (field_initializer_list)
] @fold

; Method call chains (2+ chained calls)
; simple: foo.bar().baz()
(call_expression
  function: (field_expression
    value: (call_expression))) @fold
; generic: foo.bar().collect::<Vec<_>>()
(call_expression
  function: (generic_function
    function: (field_expression
      value: (call_expression)))) @fold
; with try: foo.bar()?.baz()
(call_expression
  function: (field_expression
    value: (try_expression
      (call_expression)))) @fold
; with await: foo.bar().await.baz()
(call_expression
  function: (field_expression
    value: (await_expression
      (call_expression)))) @fold

; Other
[
  (macro_invocation)
  (token_tree)
  (use_declaration)
  (block_comment)
] @fold
