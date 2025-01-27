[
 "func"
 "let"
 "return"
] @keyword

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

(identifier) @variable

(enumerator) @constant
(null) @constant.builtin

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

(import_decl "import" @keyword.import)

(string_literal) @string
(escape_sequence) @string.escape

(number_literal) @number

(char_literal) @character

(comment) @comment @spell
