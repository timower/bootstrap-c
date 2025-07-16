import libc;

enum TokenKind {
  TOK_EOF,

  // constants
  IDENTIFIER,
  CONSTANT,
  STRING_LITERAL,
  INT2,
  COMMENT,

  // keywords
  CONTINUE,
  DEFAULT,
  EXTERN,
  SIZEOF,
  TYPEOF,
  STRUCT,
  SWITCH,
  RETURN,
  IMPORT,
  CONST,
  WHILE,
  BREAK,
  UNION,
  VOID,
  BOOL,
  TRUE,
  FALSE,
  ENUM,
  CASE,
  ELSE,
  FUNC,

  // operators
  LEFT_ASSIGN,
  RIGHT_ASSIGN,
  ELLIPSIS,
  FOR,
  LET,
  SCOPE,
  PTR_OP,
  INC_OP,
  DEC_OP,
  LEFT_OP,
  RIGHT_OP,
  LE_OP,
  GE_OP,
  EQ_OP,
  NE_OP,
  AND_OP,
  OR_OP,
  MUL_ASSIGN,
  DIV_ASSIGN,
  MOD_ASSIGN,
  ADD_ASSIGN,
  SUB_ASSIGN,
  AND_ASSIGN,
  XOR_ASSIGN,
  OR_ASSIGN,
  IF,
  AS,
  SEMICOLON,
  OPEN_BRACE,
  CLOSE_BRACE,
  COMMA,
  COLON,
  EQ,
  OPEN_PAREN,
  CLOSE_PAREN,
  OPEN_BRACKET,
  CLOSE_BRACKET,
  DOT,
  AND,
  BANG,
  TILDE,
  MINUS,
  PLUS,
  STAR,
  SLASH,
  PERCENT,
  LESS,
  GREATER,
  HAT,
  PIPE,
  QUESTION,
}

let tokens: const i8*[] = {
  "EOF", "IDENT", "CONST", "STR", "INT", "COMMENT",
  "continue", "default", "extern", "sizeof", "typeof", "struct", "switch", "return",
  "import", "const", "while", "break", "union", "void", "bool", "true", "false",
  "enum", "case", "else", "func", "<<=", ">>=", "...",
  "for", "let", "::", "->", "++", "--",
  "<<", ">>", "<=", ">=", "==", "!=",
  "&&", "||", "*=", "/=", "%=", "+=",
  "-=", "&=", "^=", "|=", "if", "as",
  ";", "{", "}", ",", ":", "=",
  "(", ")", "[", "]", ".", "&",
  "!", "~", "-", "+", "*", "/",
  "%", "<", ">", "^", "|", "?",
};

struct SourceLoc {
  data: i8*;
  fileName: i8*;
  line: i32;
  column: i32;
  // TODO: add import location.
}

struct Token {
  kind: TokenKind;
  len: i32;
  location: SourceLoc*;
}

func tokCmp(one: Token, two: Token) -> bool {
  if (one.kind != two.kind) {
    return false;
  }

  if (one.len != two.len) {
    return false;
  }

  let len = one.len as u32;
  return memcmp(one.location->data, two.location->data, len as u64) == 0;
}

func tokCmpStr(one: Token, str: const i8*) -> bool {
  let len = strlen(str);
  if (one.len != len as i32) {
    return false;
  }

  return memcmp(one.location->data, str, len as u64) == 0;
}

func newInternalToken(bufSize: u64) -> Token {
  let alloc = malloc(sizeof(SourceLoc) + bufSize);
  let loc = alloc as SourceLoc*;
  loc->data = (alloc as i8*) + sizeof(SourceLoc);
  loc->fileName = "<builtin>";
  loc->line = 1;
  loc->column = 1;
  return Token {
    kind = TokenKind::IDENTIFIER,
    len = 0,
    location = loc,
  };
}

func printLoc(loc: SourceLoc*) {
  if (loc == null) {
    fprintf(getStderr(), "%s:%d:%d: ", "<null>", 0, 0);
  } else {
    fprintf(getStderr(), "%s:%d:%d: ", loc->fileName, loc->line, loc->column);
  }
}
