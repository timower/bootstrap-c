import libc;

enum TokenKind {
  TOK_EOF,

  // clang-format off
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
  QUESTION,  // clang-format on
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

struct TokenHashEntry {
  hash: i64;
  kind: TokenKind;
  len: i32;
}

let tokenHashes: TokenHashEntry[128];

const tokenCount = sizeof(typeof(tokens)) / sizeof(typeof(tokens[0]));

func packTokenHash(str: const i8*, len: i32) -> i64 {
  if (len > 7) {
    return 0;    // Fallback for tokens longer than 7 chars
  }

  let result: i64 = len as i64;
  for (let i = 0; i < len; i++) {
    result |= (*(str + i) as i64) << (8 * (i + 1) as i64);
  }
  return result;
}

func initTokenHashes() {
  for (let i = 0; i < tokenCount; i++) {
    let len = strlen(tokens[i]) as i32;
    tokenHashes[i].hash = packTokenHash(tokens[i], len);
    tokenHashes[i].kind = i as enum TokenKind;
    tokenHashes[i].len = len;
  }
}

struct Token {
  kind: TokenKind;

  data: i8*;
  end: i8*;
}
