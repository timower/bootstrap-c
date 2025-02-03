import state;
import token;


// base_type := int2 | 'void' | 'struct' ident | 'enum' ident | ident
// type := const? base_type ('*' | '[' int? ']' )*
func parseType(state: ParseState*) -> Type* {
  let type = newType(TypeKind::Void {});

  if (match(state, TokenKind::CONST)) {
    getNextToken(state);
    type->isConst = true;
  }

  if (match(state, TokenKind::INT2)) {
    let isSigned = *state->curToken.data == 105;
    let end = state->curToken.end;
    let size = strtol(state->curToken.data + 1, &end, 10) as i32;
    getNextToken(state);
    type->kind = TypeKind::Int {
      size = size,
      isSigned = isSigned,
    };
  } else if (match(state, TokenKind::VOID)) {
    getNextToken(state);
  } else if (match(state, TokenKind::BOOL)) {
    getNextToken(state);
    type->kind = TypeKind::Bool {};
  } else if (match(state, TokenKind::STRUCT)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    type->kind = TypeKind::Struct {
      tag = getNextToken(state),
    };
  } else if (match(state, TokenKind::ENUM)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    type->kind = TypeKind::Enum {
      tag = getNextToken(state),
    };
  } else if (match(state, TokenKind::UNION)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    type->kind = TypeKind::Union {
      tag = getNextToken(state),
    };
  } else if (match(state, TokenKind::IDENTIFIER)) {
    type->kind = TypeKind::Tag {
      tag = getNextToken(state),
    };
  } else {
    failParse(state, "Unknown type");
    return null;
  }

  if (match(state, TokenKind::SCOPE)) {
    getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);

    let tagPtr = type->kind as TypeKind::Tag*;
    tagPtr->parent = tagPtr->tag;
    tagPtr->tag = getNextToken(state);
  }

  // parse type suffixes (pointers & arrays)
  while (true) {
    if (match(state, TokenKind::STAR)) {
      getNextToken(state);
      let ptrType = newType(TypeKind::Pointer {
        pointee = type,
      });
      type = ptrType;
    } else if (match(state, TokenKind::OPEN_BRACKET)) {
      getNextToken(state);

      let size = -1;
      if (match(state, TokenKind::CONSTANT)) {
        size = parseInteger(state->curToken);
        getNextToken(state);
      }

      let arrayType = newType(TypeKind::Array {
        size = size,
        element = type,
      });

      expect(state, TokenKind::CLOSE_BRACKET);
      getNextToken(state);

      type = arrayType;
    } else {
      break;
    }
  }

  return type;
}
