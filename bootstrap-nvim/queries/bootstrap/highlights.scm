[
 "func"
 "let"
] @keyword

"return" @keyword.return

[
 "struct"
 "enum"
 "union"
] @keyword.type

[
  "while"
  "for"
  "break"
] @keyword.repeat

[
  "if"
  "else"
  "case"
  "switch"
  "default"
] @keyword.conditional

"sizeof" @keyword.operator

[
  "->"
  "="
  "-"
  "*"
  "/"
  "+"
  "%"
  "~"
  "|"
  "&"
  "^"
  "<<"
  ">>"
  "->"
  "."
  "<"
  "<="
  ">="
  ">"
  "=="
  "!="
  "!"
  "&&"
  "||"
  "-="
  "+="
  "*="
  "/="
  "%="
  "|="
  "&="
  "^="
  ">>="
  "<<="
  "--"
  "++"
] @operator

[
  ";"
  ":"
  "::"
  ","
] @punctuation.delimiter

"..." @punctuation.special

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

(comma_expression
  "," @operator)

(conditional_expression
  [
    "?"
    ":"
  ] @keyword.conditional.ternary)

(cast_expression
  "as" @keyword.operator)

(identifier) @variable

(enumerator) @constant
(builtin_constant) @constant.builtin

[
 "true"
 "false"
] @boolean

(call_expression
  function: (identifier) @function.call)

(func_decl
  (identifier) @function)

(member_expression (expression)
                   (identifier) @variable.member)

(parameter (identifier) @variable.parameter
           (type))

(struct_field (identifier) @variable.member
              (type))

(type) @type
(type_identifier) @type
(primitive_type) @type.builtin

(type "const") @type.modifier
(let_decl "const") @keyword
(let_expression "const") @keyword

[
 "extern"
] @keyword.modifier

(import_decl "import" @keyword.import)

(string_literal) @string
(escape_sequence) @string.escape

(number_literal) @number

(char_literal) @character

(comment) @comment @spell
