(comment) @comment

[
  (tag_name)
  (nesting_selector)
  (universal_selector)
] @tag

[
  "~"
  ">"
  "+"
  "-"
  "*"
  "/"
  "="
  "^="
  "|="
  "~="
  "$="
  "*="
] @operator

[
  "and"
  "or"
  "not"
  "only"
  "when"
] @keyword.operator

(attribute_selector (plain_value) @string)

(property_name) @variable.other.member
(plain_value) @constant

((property_name) @variable
  (#match? @variable "^--"))
((plain_value) @variable
  (#match? @variable "^--"))

(class_name) @label
(feature_name) @variable.other.member
(function_name) @function
(id_name) @label
(namespace_name) @namespace

(attribute_name) @attribute
(pseudo_element_selector (tag_name) @attribute)
(pseudo_class_selector (class_name) @attribute)

[
  "@charset"
  "@import"
  "@media"
  "@namespace"
  "@plugin"
  "@supports"
  "from"
  "to"
  (at_keyword)
  (important_value)
  (keyword)
  (keyword_query)
  (keyframes_name)
] @keyword

(variable_name) @variable
(variable_def (variable) @variable)
(expand_variable) @variable
(interpolated_variable) @variable
(params (variable) @variable.parameter)
(params (keyword_param (variable) @variable.parameter))

["true" "false"] @constant.builtin.boolean

(string_value) @string
(color_value) @string.special

(integer_value) @constant.numeric.integer
(float_value) @constant.numeric.float
(unit) @type

[
  "#"
  "."
] @punctuation

[
  ")"
  "("
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ";"
  ":"
  "::"
] @punctuation.delimiter
