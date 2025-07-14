import state;
import util;

let intTypes: const i8*[] = {
  "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
};

const intTypeCount = (sizeof(typeof(intTypes)) / sizeof(typeof(intTypes[0])));

let intTypeHashes: i64[8];

func initIntTypeHashes() {
  for (let i = 0; i < intTypeCount; i++) {
    let len = strlen(intTypes[i]) as i32;
    intTypeHashes[i] = packTokenHash(intTypes[i], len);
  }
}

func initTokenSystem() {
  initTokenHashes();
  initIntTypeHashes();
}

func iseol(c: i32) -> bool {
  return c == '\n' || c == '\r';
}


// Returns the current character and advances the current pointer.
func nextChar(state: ParseState*) -> i32 {
  if (state->current >= state->end) {
    return -1;
  }

  let result = *state->current as i32;
  state->current++;

  if (iseol(result)) {
    state->line++;
    state->lineStart = state->current;
  }

  return result;
}


// Returns the current character without advancing
func peekChar(state: ParseState*) -> i32 {
  if (state->current >= state->end) {
    return -1;
  }
  return *state->current as i32;
}


/// True if the current character is an EOL character
func is_space(c: i32) -> bool {
  return iseol(c) || c == ' ' || c == '\t';
}

func is_alpha(c: i32) -> bool {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

func is_digit(c: i32) -> bool {
  return c >= '0' && c <= '9';
}

func is_alnum(c: i32) -> bool {
  return is_digit(c) || is_alpha(c);
}

func getToken(state: ParseState*) -> Token {
  let tokenStart = state->current;
  let lastChar = nextChar(state);

  let token = Token {};

  // Eat whitespace
  while (is_space(lastChar)) {
    tokenStart = state->current;
    lastChar = nextChar(state);
  }

  if (lastChar == -1) {
    token.kind = TokenKind::TOK_EOF;
    return token;
  }

  // identifier [a-zA-Z][a-zA-Z0-9]*
  if (is_alpha(lastChar) || lastChar == '_') {
    while (is_alnum(peekChar(state)) || peekChar(state) == '_') {
      nextChar(state);
    }

    token.data = tokenStart;
    token.end = state->current;    // one past the end!

    // Check if it's a keyword using hash lookup.
    let tokenHash = getTokenHash(token);
    if (tokenHash == 0) {
      token.kind = TokenKind::IDENTIFIER;
      return token;
    }
    for (let i = TokenKind::CONTINUE as i32; i < tokenCount; i++) {
      if (tokenHashes[i].hash == tokenHash) {
        token.kind = tokenHashes[i].kind;
        return token;
      }
    }

    // i32 types [iu](8|16|32|64) using hash lookup
    for (let i = 0; i < intTypeCount; i++) {
      if (intTypeHashes[i] == tokenHash) {
        token.kind = TokenKind::INT2;
        return token;
      }
    }

    token.kind = TokenKind::IDENTIFIER;
    return token;
  }

  if (lastChar == '\'') {
    while (peekChar(state) != '\'') {
      let next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
    }
    token.data = tokenStart;
    nextChar(state);    // eat closing '
    token.end = state->current;
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  if (lastChar == '"') {
    while (peekChar(state) != '"') {
      let next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
    }
    token.data = tokenStart + 1;    // eat the starting "
    token.end = state->current;

    nextChar(state);    // eat closing "
    token.kind = TokenKind::STRING_LITERAL;
    return token;
  }

  if (is_digit(lastChar) || (lastChar == '-' && is_digit(peekChar(state)))) {
    if (lastChar == '0' && peekChar(state) == 'x') {
      nextChar(state);
      while (is_digit(peekChar(state))
          || (peekChar(state) >= 'a' && peekChar(state) <= 'f')
          || (peekChar(state) >= 'A' && peekChar(state) <= 'F')) {
        nextChar(state);
      }
    } else {
      while (is_digit(peekChar(state))
          || peekChar(state) == 'o'
          || peekChar(state) == 'b') {
        nextChar(state);
      }
    }
    token.data = tokenStart;
    token.end = state->current;
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  // pre-processor
  if (lastChar == '#') {
    while (!iseol(peekChar(state))) {
      nextChar(state);
    }
    return getToken(state);
  }

  // Comments //
  if (lastChar == '/' && peekChar(state) == '/') {
    while (!iseol(peekChar(state)) && peekChar(state) != -1) {
      nextChar(state);
    }

    if (state->options.concrete) {
      token.kind = TokenKind::COMMENT;
      token.data = tokenStart;
      token.end = state->current;
      return token;
    } else {
      return getToken(state);
    }
  }

  // Assume operator - try different lengths for hash lookup
  let remaining = state->end - tokenStart;
  for (let i = TokenKind::LEFT_ASSIGN as i32; i < tokenCount; i++) {
    let len = tokenHashes[i].len;
    if (len <= remaining as i32) {
      if (len == 1) {
        if (*tokens[i] == *tokenStart) {
          token.kind = tokenHashes[i].kind;
          token.data = tokenStart;
          state->current = tokenStart + len;
          token.end = state->current;
          return token;
        }
      }
      let opHash = packTokenHash(tokenStart, len as i32);
      if (tokenHashes[i].hash == opHash) {
        token.kind = tokenHashes[i].kind;
        token.data = tokenStart;
        state->current = tokenStart + len;
        token.end = state->current;
        return token;
      }
    }
  }

  // Check if we're at EOF before reporting unknown token
  if (state->current >= state->end) {
    token.kind = TokenKind::TOK_EOF;
    return token;
  }

  failParse(state, "Unknown token");
  token.kind = TokenKind::TOK_EOF;
  return token;
}

func getNextToken(state: ParseState*) -> Token {
  let result = state->curToken;

  let token = getToken(state);
  while (token.kind == TokenKind::COMMENT) {
    let comment = newComment(token);
    comment->location = getLocation(state);

    if (state->lastComment != null) {
      state->lastComment->next = comment;
    }
    state->lastComment = comment;
    if (state->comments == null) {
      state->comments = comment;
    }

    token = getToken(state);
  }
  state->curToken = token;

  return result;
}

func parseInteger(state: ParseState*, token: Token) -> i32 {
  let start = token.data;
  if (*start == '\'') {
    let next = *(start + 1);
    if (next == '\\') {
      return getEscaped(*(start + 2)) as i32;
    } else {
      return next as i32;
    }
  }

  let base = 10;
  if (*start == '0') {
    switch (*(start + 1) as i32) {
      case 'x':
        base = 16;
        start += 2;
      case 'o':
        base = 8;
        start += 2;
      case 'b':
        base = 2;
        start += 2;
      default:
        break;
    }
  }

  let endp = token.end;
  let num = strtol(start, &endp, base) as i32;
  if (endp != token.end) {
    failParse(state, "Invalid integer");
  }
  return num;
}
