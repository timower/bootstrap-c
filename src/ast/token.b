import libc;
import util;

enum TokenKind {
  TOK_EOF,

  // constants
  IDENTIFIER,
  CONSTANT,
  STRING_LITERAL,
  INT,
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
  DEFER,
  CONST,
  WHILE,
  BREAK,
  UNION,
  IPTR,
  UPTR,
  VOID,
  BOOL,
  TRUE,
  FALSE,
  ENUM,
  CASE,
  ELSE,
  FUNC,
  FOR,
  LET,
  IF,
  AS,

  // 3-operators
  LEFT_ASSIGN,
  RIGHT_ASSIGN,
  ELLIPSIS,

  // 2-operators
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
  COLON_BRACKET,

  // 1-operators
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

let tokens: [i8][] = [
  "EOF"[:], "IDENT"[:], "CONST"[:], "STR"[:], "INT"[:], "COMMENT"[:],
  "continue"[:], "default"[:], "extern"[:], "sizeof"[:], "typeof"[:],
  "struct"[:], "switch"[:], "return"[:], "import"[:], "defer"[:], "const"[:],
  "while"[:], "break"[:], "union"[:], "iptr"[:], "uptr"[:], "void"[:],
  "bool"[:], "true"[:], "false"[:], "enum"[:], "case"[:], "else"[:], "func"[:],
  "for"[:], "let"[:], "if"[:], "as"[:], "<<="[:], ">>="[:], "..."[:], "::"[:],
  "->"[:], "++"[:], "--"[:], "<<"[:], ">>"[:], "<="[:], ">="[:], "=="[:],
  "!="[:], "&&"[:], "||"[:], "*="[:], "/="[:], "%="[:], "+="[:], "-="[:],
  "&="[:], "^="[:], "|="[:], ":["[:], ";"[:], "{"[:], "}"[:], ","[:], ":"[:],
  "="[:], "("[:], ")"[:], "["[:], "]"[:], "."[:], "&"[:], "!"[:], "~"[:],
  "-"[:], "+"[:], "*"[:], "/"[:], "%"[:], "<"[:], ">"[:], "^"[:], "|"[:], "?"[:],
];

struct SourceLoc {
  fileName: i8*;
  line: i32;
  column: i32;
  // TODO: add import location.
}

struct Token {
  kind: TokenKind;
  data: [i8];  // TODO: move to sourceoc?
  location: SourceLoc*;
}

struct TokenHashEntry {
  hash: i64;
  data: [i8];
}


let tokenHashes: TokenHashEntry[128];

let intTypes: const [i8][] = [
  "i8"[:], "i16"[:], "i32"[:], "i64"[:], "u8"[:], "u16"[:], "u32"[:], "u64"[:],
];

let intTypeHashes: i64[8];


func packTokenHash(str: [i8]) -> i64 {
  if (str.len > 8) {
    return 0;    // Fallback for tokens longer than 8 chars
  }

  let strPtr = &str[0] as void*;
  let result: i64 = *(strPtr as i64*);

  if (str.len != 8) {
    let ulen = str.len as u32;
    let len8 = (ulen as u64) << 3;
    result &= (((1 as u64) << len8) - 1) as i64;
  }

  return result;
}

func initTokenHashes() {
  for (let i = 0; i < tokens.len; i++) {
    let data = tokens[i];
    tokenHashes[i].data = data;
    tokenHashes[i].hash = packTokenHash(data);
    if (tokenHashes[i].hash == 0) {
      unreachable("Token too long!");
    }
  }
}

func initIntTypeHashes() {
  for (let i = 0; i < intTypes.len; i++) {
    intTypeHashes[i] = packTokenHash(intTypes[i]);
  }
}

func initTokenSystem() {
  initTokenHashes();
  initIntTypeHashes();
}


func tokCmp(one: Token, two: Token) -> bool {
  if (one.kind != two.kind) {
    return false;
    // opt: Comparing kind first is an optimization.
  }

  if (one.data.len != two.data.len) {
    return false;
  }

  let len = one.data.len as u32;
  return memcmp(&one.data[0], &two.data[0], len as uptr) == 0;
}

func tokCmpStr(one: Token, str: [i8]) -> bool {
  if (one.data.len != str.len as i32) {
    return false;
  }

  return memcmp(&one.data[0], &str[0], str.len as uptr) == 0;
}

func getTokenHash(token: Token) -> i64 {
  return packTokenHash(token.data);
}

func tokCmpHash(token: Token, hash: i64) -> bool {
  return getTokenHash(token) == hash;
}

func newInternalToken(allocator: Allocator*, bufSize: uptr) -> Token {
  let alloc = alloc(allocator, bufSize as iptr + sizeof(SourceLoc));
  let loc = alloc as SourceLoc*;
  let data = (alloc as i8*)[sizeof(SourceLoc):sizeof(SourceLoc) + bufSize];
  let str: [i8] = "<builtin>";
  loc->fileName = &str[0];
  loc->line = 1;
  loc->column = 1;
  return Token {
    kind = TokenKind::IDENTIFIER,
    data = data,
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
