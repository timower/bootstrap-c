import state;
import token;


// number := [0-9]+ | '[\n\t\r\\'"]' | '.'
func parseNumber(state: ParseState*) -> ExprAST* {
  let token = state->curToken;
  let value = parseInteger(state, token);
  let result = newCurLocExpr(state, ExprKind::Int {
    value = value,
    token = token,
  });
  result->type = getInt32();

  getNextToken(state);
  return result;
}


// string := '"' [^"]* '"'
func parseString(state: ParseState*) -> ExprAST* {
  let identifier = state->curToken;
  let result = newCurLocExpr(state, ExprKind::Str {
    identifier = identifier,
  });
  getNextToken(state);
  return result;
}


// structInit = '{' ( ident '=' cond ','  )* ','? '}'
func parseStructInit(state: ParseState*) -> ExprAST* {
  let expr = newCurLocExpr(state, ExprKind::Struct {
    identifier = Token {},
    parent = Token {},
    fieldIndices = null,
  });
  getNextToken(state);  // eat '{'

  let fieldIndex: FieldIndex* = null;
  let lastFieldIndex: FieldIndex* = null;

  while (!match(state, TokenKind::CLOSE_BRACE)) {
    expect(state, TokenKind::IDENTIFIER);
    let fieldName = getNextToken(state);

    expect(state, TokenKind::EQ);
    getNextToken(state);

    let fieldExpr = parseConditional(state);

    // Create a FieldIndex entry (index will be set during sema)
    let newField = newFieldIndex(fieldName, fieldExpr);
    if (fieldIndex == null) {
      fieldIndex = newField;
      (&expr->kind as ExprKind::Struct*)->fieldIndices = fieldIndex;
    } else {
      lastFieldIndex->next = newField;
    }
    lastFieldIndex = newField;

    // close without trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);    // eat ,
  }
  getNextToken(state);  // eat }

  return expr;
}


// identExpr := identifier
//           | identifier '::' identifier
//           | identifier '{' assign* '}'
func parseIdentifierExpr(state: ParseState*) -> ExprAST* {
  let ident = getNextToken(state);

  switch (state->curToken.kind) {
    case TokenKind::OPEN_BRACE:
      let res = parseStructInit(state);
      (&res->kind as ExprKind::Struct*)->identifier = ident;
      return res;

    case TokenKind::COLON_BRACKET:
      // Parse generic instantiation foo:[T, U]
      let loc = getNextToken(state).location;      // eat :[

      let firstTypeArg: Type* = null;
      let curTypeArg: Type* = null;
      while (!match(state, TokenKind::CLOSE_BRACKET)) {
        let typeArg = parseType(state, false);

        if (firstTypeArg == null) {
          firstTypeArg = typeArg;
          curTypeArg = typeArg;
        } else {
          curTypeArg->next = typeArg;
          curTypeArg = typeArg;
        }

        if (match(state, TokenKind::CLOSE_BRACKET)) {
          break;
        }

        expect(state, TokenKind::COMMA);
        getNextToken(state);        // eat ,
      }

      expect(state, TokenKind::CLOSE_BRACKET);
      getNextToken(state);      // eat ]

      return newLocExpr(loc, ExprKind::GenericInstantiation {
        function = ident,
        typeArgs = firstTypeArg,
      });

    case TokenKind::SCOPE:
      getNextToken(state);      // eat ::

      expect(state, TokenKind::IDENTIFIER);
      let member = getNextToken(state);

      if (!match(state, TokenKind::OPEN_BRACE)) {
        let result = newLocExpr(member.location, ExprKind::Scope {
          parent = ident,
          identifier = member,
        });
        return result;
      }

      let res = parseStructInit(state);
      let structKind = &res->kind as ExprKind::Struct*;
      structKind->identifier = member;
      structKind->parent = ident;
      return res;

    default:
      let result = newLocExpr(ident.location, ExprKind::Variable {
        identifier = ident,
      });
      return result;
  }
}


// paren := '(' expression ')'
func parseParen(state: ParseState*) -> ExprAST* {
  // eat (
  let loc = getNextToken(state).location;

  let expr = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);  // eat )

  // if (!state->options.concrete) {
  //   return expr;
  // }
  let res = newLocExpr(loc, ExprKind::Paren {
    expr = expr,
  });
  return res;
}


// primary := identExpr
//          | number
//          | string
//          | paren
func parsePrimary(state: ParseState*) -> ExprAST* {
  switch (state->curToken.kind) {
    case TokenKind::TRUE, TokenKind::FALSE:
      let token = getNextToken(state);
      let value = token.kind == TokenKind::TRUE ? 1 : 0;
      let expr = newLocExpr(token.location, ExprKind::Int {
        value = value,
        token = token,
      });
      expr->type = getBool();
      return expr;
    case TokenKind::IDENTIFIER:
      return parseIdentifierExpr(state);
    case TokenKind::CONSTANT:
      return parseNumber(state);
    case TokenKind::STRING_LITERAL:
      return parseString(state);
    case TokenKind::OPEN_PAREN:
      return parseParen(state);
    case TokenKind::OPEN_BRACKET:
      return parseInitializer(state);

    default:
      printToken(state->curToken);
      failParse(state, "Unknown primary expression");
      return null;
  }
}


// index := lhs '[' expression ']'
//        | lhs '[' expression? ':' expression? ']'
func parseIndex(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let loc = getNextToken(state).location;  // eat [

  if (match(state, TokenKind::COLON)) {
    getNextToken(state);    // eat :

    let end: ExprAST* = null;
    if (!match(state, TokenKind::CLOSE_BRACKET)) {
      end = parseExpression(state);
    }
    let expr = newLocExpr(loc, ExprKind::SliceIndex {
      slice = lhs,
      start = null,
      end = end,
    });

    expect(state, TokenKind::CLOSE_BRACKET);
    getNextToken(state);

    return expr;
  }

  let index = parseExpression(state);

  if (match(state, TokenKind::COLON)) {
    getNextToken(state);

    let end: ExprAST* = null;
    if (!match(state, TokenKind::CLOSE_BRACKET)) {
      end = parseExpression(state);
    }
    let expr = newLocExpr(loc, ExprKind::SliceIndex {
      slice = lhs,
      start = index,
      end = end,
    });

    expect(state, TokenKind::CLOSE_BRACKET);
    getNextToken(state);

    return expr;
  }

  let expr = newLocExpr(loc, ExprKind::Index {
    array = lhs,
    index = index,
  });

  expect(state, TokenKind::CLOSE_BRACKET);
  getNextToken(state);

  return expr;
}


// call := lhs '(' [assignment (',' assigment)*] ')'
func parseCall(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let loc = getNextToken(state).location;  // eat (

  let args: ExprAST* = null;
  let lastArg: ExprAST* = null;

  if (!match(state, TokenKind::CLOSE_PAREN)) {
    while (true) {
      let arg = parseAssignment(state);

      if (args == null) {
        args = arg;
        lastArg = arg;
      } else {
        lastArg->next = arg;
        lastArg = arg;
      }

      if (!match(state, TokenKind::COMMA)) {
        break;
      }
      getNextToken(state);
    }
  }

  let expr = newLocExpr(loc, ExprKind::Call {
    function = lhs,
    args = args,
  });

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  return expr;
}


// member := lhs ['.' | '->'] identifier
func parseMember(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let op = getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);
  let identifier = state->curToken;
  getNextToken(state);

  return newLocExpr(op.location, ExprKind::Member {
    object = lhs,
    identifier = identifier,
    op = op,
    fieldIndex = -1,
  });
}


// unary_postfix := lhs '++' | lhs '--'
func parseUnaryPostfix(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let op = getNextToken(state);

  return newLocExpr(op.location, ExprKind::Unary {
    op = op,
    postfix = lhs,
    prefix = null,
  });
}


// postfix := primary ( [index | call | member | unary_postfix] )*
func parsePostfix(state: ParseState*) -> ExprAST* {
  let expr = parsePrimary(state);

  while (true) {
    if (expr == null) {
      return expr;
    }

    switch (state->curToken.kind) {
      case TokenKind::OPEN_BRACKET:
        expr = parseIndex(state, expr);
      case TokenKind::OPEN_PAREN:
        expr = parseCall(state, expr);
      case TokenKind::DOT, TokenKind::PTR_OP:
        expr = parseMember(state, expr);
      case TokenKind::INC_OP, TokenKind::DEC_OP:
        expr = parseUnaryPostfix(state, expr);
      default:
        return expr;
    }
  }
  return expr;
}


func isUnary(tok: Token) -> bool {
  switch (tok.kind) {
    case TokenKind::INC_OP, TokenKind::DEC_OP, TokenKind::AND, TokenKind::STAR,
         TokenKind::PLUS, TokenKind::MINUS, TokenKind::TILDE, TokenKind::BANG:
      return true;
    default:
      return false;
  }
}


// unary := postfix
//        | '++' unary
//        | '--' unary
//        | '&' unary
//        | '*' unary
//        | '+' unary
//        | '-' unary
//        | '~' unary
//        | '!' unary
//        | sizeof '(' unary ')'
//        | sizeof '(' decl ')'
func parseUnary(state: ParseState*) -> ExprAST* {
  if (isUnary(state->curToken)) {
    let op = getNextToken(state);
    let prefix = parseUnary(state);
    let expr = newLocExpr(op.location, ExprKind::Unary {
      op = op,
      postfix = null,
      prefix = prefix,
    });
    return expr;
  }

  if (match(state, TokenKind::SIZEOF)) {
    let loc = getNextToken(state).location;

    expect(state, TokenKind::OPEN_PAREN);
    getNextToken(state);

    let typeArg = parseType(state, false);
    let expr = newLocExpr(loc, ExprKind::Sizeof {
      typeArg = typeArg,
      value = 0,
    });

    expect(state, TokenKind::CLOSE_PAREN);
    getNextToken(state);

    return expr;
  }

  return parsePostfix(state);
}


// cast := unary | unary 'as' type
func parseCast(state: ParseState*) -> ExprAST* {
  let lhs = parseUnary(state);

  if (!match(state, TokenKind::AS)) {
    return lhs;
  }

  let loc = getNextToken(state).location;
  let castType = parseType(state, false);
  let expr = newLocExpr(loc, ExprKind::Cast {
    expr = lhs,
    castKind = CastKind::Noop,
    fieldIndex = -1,
  });
  expr->type = castType;
  return expr;
}


// binary_rhs := lhs ( op cast )*
func parseBinOpRhs(
    state: ParseState*,
    prec: i32,
    lhs: ExprAST*
) -> ExprAST* {
  while (true) {
    let curPred = getBinOpPrecedence(state->curToken);
    if (curPred < prec) {
      return lhs;
    }

    let op = getNextToken(state);

    let rhs = parseCast(state);

    let nextPred = getBinOpPrecedence(state->curToken);
    if (curPred < nextPred) {
      rhs = parseBinOpRhs(state, curPred + 1, rhs);
    }

    let newLhs = newLocExpr(op.location, ExprKind::Binary {
      op = op,
      lhs = lhs,
      rhs = rhs,
    });

    lhs = newLhs;
  }
  return null;
}


// binary := cast (op cast)*
func parseBinOp(state: ParseState*) -> ExprAST* {
  let lhs = parseCast(state);
  return parseBinOpRhs(state, 0, lhs);
}


// conditional := binary
//              | binary '?' expression ':' conditional
func parseConditional(state: ParseState*) -> ExprAST* {
  let cond = parseBinOp(state);
  if (!match(state, TokenKind::QUESTION)) {
    return cond;
  }
  getNextToken(state);

  let trueBranch = parseExpression(state);
  expect(state, TokenKind::COLON);
  getNextToken(state);
  let falseBranch = parseConditional(state);

  let expr = newLocExpr(cond->location, ExprKind::Conditional {
    cond = cond,
    trueExpr = trueBranch,
    falseExpr = falseBranch,
  });
  return expr;
}


// expression := assignment (',' assignment)*
func parseExpression(state: ParseState*) -> ExprAST* {
  let expr = parseAssignment(state);
  while (match(state, TokenKind::COMMA)) {
    let op = getNextToken(state);
    let rhs = parseAssignment(state);

    expr = newLocExpr(op.location, ExprKind::Binary {
      op = op,
      lhs = expr,
      rhs = rhs,
    });
  }
  return expr;
}


// struct DeclAST *parseNoInitDecl(state: ParseState*);
// initializer := assignment
//             | '{' assignment (',' assignment)* ','? '}'
//             | '[' assignment (',' assignment)* ','? ']'
func parseInitializer(state: ParseState*) -> ExprAST* {
  if (!match(state, TokenKind::OPEN_BRACE)
      && !match(state, TokenKind::OPEN_BRACKET)) {
    return parseAssignment(state);
  }

  let expr = newCurLocExpr(state, ExprKind::Array {
    elements = null,
  });
  getNextToken(state);  // eat '{'

  let cur: ExprAST* = null;
  let last: ExprAST* = null;
  while (true) {
    // Should be parseInitializer(state), but let's not support nested inits.
    let elem = parseAssignment(state);
    if (cur == null) {
      (&expr->kind as ExprKind::Array*)->elements = elem;
      cur = elem;
    } else {
      cur->next = elem;
      cur = elem;
    }

    // close without trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)
        || match(state, TokenKind::CLOSE_BRACKET)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);    // eat ,

    // close with trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)
        || match(state, TokenKind::CLOSE_BRACKET)) {
      break;
    }
  }
  getNextToken(state);  // eat }

  return expr;
}


// let_decl := 'let' identifier [':' type] ['=' initializer]
//           | 'const' identifier [':' type] '=' initializer
func parseVarDecl(state: ParseState*) -> DeclAST* {
  let isConst = match(state, TokenKind::CONST);
  let decl: DeclAST* = null;
  if (isConst) {
    decl = newLocDecl(state, DeclKind::Const {});
  } else {
    decl = newLocDecl(state, DeclKind::Var {});
  }
  getNextToken(state);  // eat let or const

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  if (match(state, TokenKind::COLON)) {
    getNextToken(state);
    decl->type = parseType(state, true);
  } else {
    // Without type we need an init.
    expect(state, TokenKind::EQ);
  }

  if (match(state, TokenKind::EQ)) {
    getNextToken(state);
    let init = parseInitializer(state);
    if (isConst) {
      (&decl->kind as DeclKind::Const*)->init = init;
    } else {
      (&decl->kind as DeclKind::Var*)->init = init;
    }
  }

  decl->endLocation = state->curToken.location;
  return decl;
}


// let_expr := 'let' identifier (':' type)? '=' assignment
func parseLetExpr(state: ParseState*) -> ExprAST* {
  let decl = parseVarDecl(state);
  return newLocExpr(decl->location, ExprKind::Let {
    decl = decl,
  });
}


// assignment := let_expr | conditional | conditional '=' assignment
func parseAssignment(state: ParseState*) -> ExprAST* {
  if (match(state, TokenKind::LET) || match(state, TokenKind::CONST)) {
    return parseLetExpr(state);
  }

  let lhs = parseConditional(state);
  if (!isAssign(state->curToken)) {
    return lhs;
  }

  let op = getNextToken(state);
  let rhs = parseAssignment(state);
  return newLocExpr(lhs->location, ExprKind::Binary {
    op = op,
    lhs = lhs,
    rhs = rhs,
  });
}


// base_type := int2 | 'void' | 'struct' ident | 'enum' ident
//            | ident | 'func' '(' type* ')' ('->' type)?
// type := const? base_type ('*' | '[' int? ']' )*
func parseType(state: ParseState*, allowUnsized: bool) -> Type* {
  let type = newType(TypeKind::Void {});

  let fnType: TypeKind::Func* = null;

  if (match(state, TokenKind::CONST)) {
    getNextToken(state);
    type->isConst = true;
  }
  type->location = state->curToken.location;

  if (match(state, TokenKind::INT)) {
    let data = state->curToken.data;
    let isSigned = data[0] == 'i';
    data = data[1:];
    let end = &data[data.len];
    let size = strtol(&data[0], &end, 10) as i32;
    getNextToken(state);
    type->kind = TypeKind::Int {
      size = size,
      isSigned = isSigned,
    };
  } else if (match(state, TokenKind::IPTR)) {
    getNextToken(state);
    type->kind = TypeKind::Int {
      size = -1,
      isSigned = true,
      isPtr = true,
    };
  } else if (match(state, TokenKind::UPTR)) {
    getNextToken(state);
    type->kind = TypeKind::Int {
      size = -1,
      isSigned = false,
      isPtr = true,
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
  } else if (match(state, TokenKind::TYPEOF)) {
    getNextToken(state);
    expect(state, TokenKind::OPEN_PAREN);
    getNextToken(state);

    let expr = parseExpression(state);

    expect(state, TokenKind::CLOSE_PAREN);
    getNextToken(state);

    type->kind = TypeKind::Typeof {
      expr = expr,
    };
  } else if (match(state, TokenKind::FUNC)) {
    getNextToken(state);
    type->kind = TypeKind::Func {};
    fnType = type->kind as TypeKind::Func*;
  } else if (match(state, TokenKind::IDENTIFIER)) {
    type->kind = TypeKind::Tag {
      tag = getNextToken(state),
    };

    if (match(state, TokenKind::SCOPE)) {
      getNextToken(state);
      expect(state, TokenKind::IDENTIFIER);

      let tagPtr = type->kind as TypeKind::Tag*;
      tagPtr->parent = tagPtr->tag;
      tagPtr->tag = getNextToken(state);
    }
  } else if (match(state, TokenKind::OPEN_BRACKET)) {
    getNextToken(state);
    type->kind = TypeKind::Slice {
      element = parseType(state, allowUnsized),
    };

    expect(state, TokenKind::CLOSE_BRACKET);
    getNextToken(state);    // eat ']'
  } else {
    failParse(state, "Unknown type");
    return null;
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

      if (allowUnsized && match(state, TokenKind::CLOSE_BRACKET)) {
        getNextToken(state);
        type = newType(TypeKind::Array {
          size = -1,
          element = type,
        });
      } else {
        // TODO: allow any constant expression.
        expect(state, TokenKind::CONSTANT);
        let size = parseInteger(state, state->curToken);
        getNextToken(state);

        type = newType(TypeKind::Array {
          size = size,
          element = type,
        });

        expect(state, TokenKind::CLOSE_BRACKET);
        getNextToken(state);
      }
    } else {
      break;
    }
  }

  if (fnType == null) {
    return type;
  }

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);

  let args = null as Type*;
  let isVarargs = false;

  if (!match(state, TokenKind::CLOSE_PAREN)) {
    if (match(state, TokenKind::ELLIPSIS)) {
      getNextToken(state);
      isVarargs = true;
    } else {
      args = parseType(state, allowUnsized);
      let currentArg = args;

      while (match(state, TokenKind::COMMA)) {
        getNextToken(state);

        if (match(state, TokenKind::ELLIPSIS)) {
          getNextToken(state);
          isVarargs = true;
          break;
        }

        let nextArg = parseType(state, allowUnsized);
        currentArg->next = nextArg;
        currentArg = nextArg;
      }
    }
  }

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  let returnType = newType(TypeKind::Void {});
  if (match(state, TokenKind::PTR_OP)) {
    getNextToken(state);
    returnType = parseType(state, allowUnsized);
  }

  fnType->result = returnType;
  fnType->args = args;
  fnType->isVarargs = isVarargs;

  return type;
}
