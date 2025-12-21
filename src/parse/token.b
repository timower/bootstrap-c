import state;
import util;

func iseol(c: i32) -> bool {
  return c == '\n' || c == '\r';
}


// Returns the current character and advances the current pointer.
func nextChar(state: ParseState*) -> i32 {
  if (state->current >= state->buf.len) {
    return -1;
  }

  let result = state->buf[(state->current)] as i32;
  state->current++;

  if (iseol(result)) {
    state->line++;
    state->lineStart = state->current;
  }

  return result;
}


// Returns the current character without advancing
func peekChar(state: ParseState*) -> i32 {
  if (state->current >= state->buf.len) {
    return -1;
  }
  return state->buf[(state->current)] as i32;
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

func newLocation(state: ParseState*, start: i32) -> SourceLoc* {
  if (state->sourceSlabs.len == 0) {
    let slabs = calloc(source_slab_size, sizeof(SourceLoc)) as SourceLoc*;
    state->sourceSlabs = slabs[:source_slab_size];
  }

  let result = &state->sourceSlabs[0];
  state->sourceSlabs = state->sourceSlabs[1:];

  result->column = start - state->lineStart + 1;
  result->line = state->line;
  result->fileName = state->fileName;
  return result;
}

func makeToken(state: ParseState*, tokenStart: i32) -> Token {
  return Token {
    location = newLocation(state, tokenStart),
    data = state->buf[tokenStart:(state->current)],
  };
}

func makeEof(state: ParseState*, tokenStart: i32) -> Token {
  return Token {
    kind = TokenKind::TOK_EOF,
    location = newLocation(state, tokenStart),
  };
}

func getToken(state: ParseState*) -> Token {
  let tokenStart = state->current;
  let lastChar = nextChar(state);

  // Eat whitespace
  while (is_space(lastChar)) {
    tokenStart = state->current;
    lastChar = nextChar(state);
  }

  if (lastChar == -1) {
    return makeEof(state, tokenStart);
  }

  // identifier [a-zA-Z][a-zA-Z0-9]*
  if (is_alpha(lastChar) || lastChar == '_') {
    while (is_alnum(peekChar(state)) || peekChar(state) == '_') {
      nextChar(state);
    }

    let token = makeToken(state, tokenStart);

    // Check if it's a keyword using hash lookup.
    let tokenHash = getTokenHash(token);
    if (tokenHash == 0) {
      token.kind = TokenKind::IDENTIFIER;
      return token;
    }

    for (let i = TokenKind::CONTINUE as i32; i <= TokenKind::AS as i32; i++) {
      if (tokenHashes[i].hash == tokenHash) {
        token.kind = i as enum TokenKind;
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
      if (next == -1) {
        return makeEof(state, tokenStart);
      }
    }
    nextChar(state);    // eat closing '
    let token = makeToken(state, tokenStart);
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  if (lastChar == '"') {
    while (peekChar(state) != '"') {
      let next = nextChar(state);
      if (next == '\\') {
        nextChar(state);
      }
      if (next == -1) {
        return makeEof(state, tokenStart);
      }
    }
    let token = makeToken(state, tokenStart + 1);    // eat the starting "
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
    let token = makeToken(state, tokenStart);
    token.kind = TokenKind::CONSTANT;
    return token;
  }

  // pre-processor
  if (lastChar == '#') {
    while (!iseol(peekChar(state)) && peekChar(state) != -1) {
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
      let token = makeToken(state, tokenStart);
      token.kind = TokenKind::COMMENT;
      return token;
    } else {
      return getToken(state);
    }
  }

  // Assume operator - try different lengths for hash lookup
  let rest = state->buf[tokenStart:];
  for (let i = TokenKind::LEFT_ASSIGN as i32; i < tokenCount; i++) {
    let token = tokenHashes[i];
    let len = token.data.len;
    if (len <= rest.len) {
      let match = token.data[0] == rest[0];
      if (len > 1) {
        match &= (token.data[1] == rest[1]);
      }
      if (len > 2) {
        match &= (token.data[2] == rest[2]);
      }

      if (match) {
        state->current = tokenStart + len;
        let token = makeToken(state, tokenStart);
        token.kind = i as enum TokenKind;
        return token;
      }
    }
  }

  // Check if we're at EOF before reporting unknown token
  if (state->current >= state->buf.len) {
    return makeEof(state, tokenStart);
  }

  failParse(state, "Unknown token");
  return Token {};
}

func getNextToken(state: ParseState*) -> Token {
  let result = state->curToken;

  let token = getToken(state);
  while (token.kind == TokenKind::COMMENT) {
    let comment = newComment(token);
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
  if (token.data[0] == '\'') {
    if (token.data[1] == '\\') {
      return getEscaped(token.data[2]) as i32;
    } else {
      return token.data[1] as i32;
    }
  }

  let data = token.data;
  let base = 10;
  if (data[0] == '0') {
    switch (data[1] as i32) {
      case 'x':
        base = 16;
        data = data[2:];
      case 'o':
        base = 8;
        data = data[2:];
      case 'b':
        base = 2;
        data = data[2:];
      default:
        break;
    }
  }

  let endp = &data[(data.len)];
  let num = strtol(&data[0], &endp, base) as i32;
  if (endp != &data[(data.len)]) {
    failParse(state, "Invalid integer");
  }
  return num;
}
