import state;
import token;
import type;


// number := [0-9]+ | '[\n\t\r\\'"]' | '.'
func parseNumber(state: ParseState*) -> ExprAST* {
  let token = state->curToken;
  let value = parseInteger(state, token);
  let result = newLocExpr(state, ExprKind::Int {
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
  let result = newLocExpr(state, ExprKind::Str {
    identifier = identifier,
  });
  getNextToken(state);
  return result;
}


// structInit = '{' ( ident '=' cond ','  )* ','? '}'
func parseStructInit(state: ParseState*) -> ExprAST* {
  let expr = newLocExpr(state, ExprKind::Struct {
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
  let loc = getLocation(state);
  let ident = getNextToken(state);

  switch (state->curToken.kind) {
    case TokenKind::OPEN_BRACE:
      let res = parseStructInit(state);
      (&res->kind as ExprKind::Struct*)->identifier = ident;
      return res;

    case TokenKind::SCOPE:
      getNextToken(state);      // eat ::

      expect(state, TokenKind::IDENTIFIER);
      let loc = getLocation(state);
      let member = getNextToken(state);

      if (!match(state, TokenKind::OPEN_BRACE)) {
        let result = newLocExpr(state, ExprKind::Scope {
          parent = ident,
          identifier = member,
        });
        result->location = loc;
        return result;
      }

      let res = parseStructInit(state);
      let structKind = &res->kind as ExprKind::Struct*;
      structKind->identifier = member;
      structKind->parent = ident;
      return res;

    default:
      let result = newLocExpr(state, ExprKind::Variable {
        identifier = ident,
      });
      result->location = loc;
      return result;
  }
}


// paren := '(' expression ')'
func parseParen(state: ParseState*) -> ExprAST* {
  getNextToken(state);  // eat (

  let expr = parseExpression(state);

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);  // eat )
  if (!state->options.concrete) {
    return expr;
  }

  let res = newLocExpr(state, ExprKind::Paren {
    expr = expr,
  });
  res->location = expr->location;  // TODO: use location of (
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
      let expr = newLocExpr(state, ExprKind::Int {
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
    default:
      failParse(state, "Unknow primary expression");
      return null;
  }
}


// index := lhs '[' expression ']'
func parseIndex(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  getNextToken(state);  // eat [

  let index = parseExpression(state);
  let expr = newLocExpr(state, ExprKind::Index {
    array = lhs,
    index = index,
  });

  expect(state, TokenKind::CLOSE_BRACKET);
  getNextToken(state);

  return expr;
}


// call := lhs '(' [assignment (',' assigment)*] ')'
func parseCall(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  getNextToken(state);  // eat (

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

  let expr = newLocExpr(state, ExprKind::Call {
    function = lhs,
    args = args,
  });

  expect(state, TokenKind::CLOSE_PAREN);
  getNextToken(state);

  return expr;
}


// member := lhs ['.' | '->'] identifier
func parseMember(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let op = state->curToken;
  getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);
  let identifier = state->curToken;
  getNextToken(state);

  let expr = newLocExpr(state, ExprKind::Member {
    object = lhs,
    identifier = identifier,
    op = op,
    fieldIndex = -1,
  });
  return expr;
}


// unary_postfix := lhs '++' | lhs '--'
func parseUnaryPostfix(state: ParseState*, lhs: ExprAST*) -> ExprAST* {
  let op = state->curToken;
  getNextToken(state);

  let expr = newLocExpr(state, ExprKind::Unary {
    op = op,
    postfix = lhs,
    prefix = null,
  });
  return expr;
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
    let op = state->curToken;
    getNextToken(state);
    let prefix = parseUnary(state);
    let expr = newLocExpr(state, ExprKind::Unary {
      op = op,
      postfix = null,
      prefix = prefix,
    });
    return expr;
  }

  if (match(state, TokenKind::SIZEOF)) {
    getNextToken(state);

    expect(state, TokenKind::OPEN_PAREN);
    getNextToken(state);

    let expr: ExprAST* = null;

    // TODO: fix...
    if (isDecl(state->curToken) && !match(state, TokenKind::LET)) {
      let typeArg = parseType(state);
      expr = newLocExpr(state, ExprKind::Sizeof {
        expr = null,
        typeArg = typeArg,
        value = 0,
      });
    } else {
      let innerExpr = parseUnary(state);
      expr = newLocExpr(state, ExprKind::Sizeof {
        expr = innerExpr,
        typeArg = null,
        value = 0,
      });
    }

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
  getNextToken(state);

  let castType = parseType(state);
  let expr = newLocExpr(state, ExprKind::Cast {
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

    let op = state->curToken;
    let loc = getLocation(state);
    getNextToken(state);

    let rhs = parseCast(state);

    let nextPred = getBinOpPrecedence(state->curToken);
    if (curPred < nextPred) {
      rhs = parseBinOpRhs(state, curPred + 1, rhs);
    }

    let newLhs = newLocExpr(state, ExprKind::Binary {
      op = op,
      lhs = lhs,
      rhs = rhs,
    });
    newLhs->location = loc;

    lhs = newLhs;
  }
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

  let expr = newLocExpr(state, ExprKind::Conditional {
    cond = cond,
    trueExpr = trueBranch,
    falseExpr = falseBranch,
  });
  expr->location = cond->location;
  return expr;
}


// expression := assignment (',' assignment)*
func parseExpression(state: ParseState*) -> ExprAST* {
  let expr = parseAssignment(state);
  while (match(state, TokenKind::COMMA)) {
    let op = getNextToken(state);
    let rhs = parseAssignment(state);

    expr = newLocExpr(state, ExprKind::Binary {
      op = op,
      lhs = expr,
      rhs = rhs,
    });
  }
  return expr;
}


// struct DeclAST *parseNoInitDecl(state: ParseState*);
// initializer := assignment | '{' assignment (',' assignment)* ','? '}'
func parseInitializer(state: ParseState*) -> ExprAST* {
  if (!match(state, TokenKind::OPEN_BRACE)) {
    return parseAssignment(state);
  }

  let expr = newLocExpr(state, ExprKind::Array {
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
    if (match(state, TokenKind::CLOSE_BRACE)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    let loc = getLocation(state);
    getNextToken(state);    // eat ,

    // close with trailing comma
    if (match(state, TokenKind::CLOSE_BRACE)) {
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
    decl->type = parseType(state);
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

  decl->endLocation = getLocation(state);

  return decl;
}


// let_expr := 'let' identifier (':' type)? '=' assignment
func parseLetExpr(state: ParseState*) -> ExprAST* {
  let decl = parseVarDecl(state);
  let expr = newLocExpr(state, ExprKind::Let {
    decl = decl,
  });
  return expr;
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
  let expr = newLocExpr(state, ExprKind::Binary {
    op = op,
    lhs = lhs,
    rhs = rhs,
  });
  expr->location = lhs->location;
  return expr;
}
