import state;

import expr;


func getFieldCount(decl: DeclAST*) -> i32 {
  let count = 0;

  if (decl->kind == DeclKind::ENUM) {
    for (let field = decl->fields; field != null; field = field->next) {
      count++;
    }
  } else if (decl->kind == DeclKind::UNION) {
    for (let tag = decl->subTypes; tag != null; tag = tag->next) {
      count++;
    }
  }

  return count;
}


// Semas the expression, and returns a bitset of matched field indexes.
func getFieldBitset(state: SemaState*, expr: ExprAST*) -> i32 {
  switch (expr->kind) {
    case ExprKind::SCOPE, ExprKind::MEMBER:
      return 1 << expr->value;
    case ExprKind::BINARY:
      if (expr->op.kind != TokenKind::COMMA) {
        failSemaExpr(expr, "Unsupported case expression");
      }
      return getFieldBitset(state, expr->lhs) | getFieldBitset(state, expr->rhs);
    default:
      failSemaExpr(expr, "Unsupported case expression");
  }
}

func semaCaseExpr(state: SemaState*, switchType: Type*, expr: ExprAST*) {
  switch (expr->kind) {
    case ExprKind::MEMBER:
      let scopeExpr = expr->lhs;
      let varName = expr->identifier;

      if (scopeExpr->kind != ExprKind::SCOPE) {
        failSemaExpr(scopeExpr, "Expected :: expression");
      }

      let unionName = scopeExpr->parent;
      let tagName = scopeExpr->identifier;

      let unionDecl = lookupType(state, unionName);
      if (unionDecl == null) {
        failSemaExpr(expr, "Unknown union");
      }

      let tagIdx = 0;
      let tagDecl = findTypeIdx(unionDecl->subTypes, tagName, &tagIdx);
      if (tagDecl == null) {
        failSemaExpr(expr, "Unkown tag in union");
      }

      expr->type = unionDecl->type;
      expr->value = tagIdx;

      // Make a new variable declaration.
      let varDecl = newDecl(DeclKind::VAR);
      varDecl->type = tagDecl->type;
      varDecl->name = varName;

      addLocalDecl(state, varDecl);

    case ExprKind::SCOPE:
      let decl = lookupType(state, expr->parent);
      if (decl == null) {
        failSemaExpr(expr, "Couldn't find type");
      }

      switch (decl->type->kind) {
        case TypeKind::Enum as e:
          let fieldDecl = findField(decl, expr->identifier, &expr->value);
          if (fieldDecl == null) {
            failSemaExpr(expr, " Cannot find field");
          }

          expr->type = decl->type;
        case TypeKind::Union:
          let tagDecl = findTypeIdx(
              decl->subTypes,
              expr->identifier,
              &expr->value);
          if (tagDecl == null) {
            failSemaExpr(expr, "Cannot find tag");
          }

          expr->type = decl->type;

        default:
          failSemaExpr(
              expr,
              "Expected union or enum parent for member case expr");
      }

      break;

    default:
      semaExpr(state, expr);
  }

  if (!typeEq(expr->type, switchType)) {
    failSemaExpr(expr, "case expr must match switch type");
  }
}

func semaSwitchStmt(state: SemaState*, stmt: StmtAST*) {
  semaExpr(state, stmt->expr);
  let switchType = stmt->expr->type;

  let exhaustive =
      switchType->kind as TypeKind::Enum* != null
      || switchType->kind as TypeKind::Union* != null;

  if (switchType->kind as TypeKind::Int* == null && !exhaustive) {
    printType(stmt->expr->type);
    failSemaExpr(stmt->expr, "Switch expr must be integer, enum or union");
  }

  let fieldBitSet = 0;

  for (let caseStmt = stmt->stmt; caseStmt != null;
       caseStmt = caseStmt->nextStmt) {
    let subState = newState(state);
    if (caseStmt->kind == StmtKind::CASE) {
      semaCaseExpr(&subState, switchType, caseStmt->expr);

      if (exhaustive) {
        fieldBitSet |= getFieldBitset(&subState, caseStmt->expr);
      }
    } else if (caseStmt->kind == StmtKind::DEFAULT) {
      fieldBitSet = -1;
    } else {
      failSemaStmt(caseStmt, "Unknown switch case statement");
    }

    for (let cur = caseStmt->stmt; cur != null; cur = cur->nextStmt) {
      semaStmt(&subState, cur);
    }
  }

  if (exhaustive && fieldBitSet != -1) {
    let typeTag = getTypeTag(switchType);
    let decl = lookupType(state, *typeTag);
    if (decl == null) {
      failSemaStmt(stmt, "Couldn't find enum decl");
    }
    let size = getFieldCount(decl);
    if ((1 << size) - 1 != fieldBitSet) {
      failSemaStmt(stmt, "Switch is not exhaustive");
    }
  }
}

func makeNullCmp(expr: ExprAST*) -> ExprAST* {
  let cmpExpr = newExpr(ExprKind::BINARY);
  cmpExpr->op = Token {
    kind = TokenKind::NE_OP,
  };
  cmpExpr->lhs = expr;
  cmpExpr->rhs = newExpr(ExprKind::INT);
  cmpExpr->rhs->type = expr->type;
  cmpExpr->rhs->value = 0;

  cmpExpr->type = getInt32();  // TODO: Bool.

  return cmpExpr;
}

func semaStmt(state: SemaState*, stmt: StmtAST*) {
  switch (stmt->kind) {
    case StmtKind::EXPR:
      if (stmt->expr != null) {
        semaExpr(state, stmt->expr);
      }

    case StmtKind::RETURN:
      if (stmt->expr == null && state->result->kind as TypeKind::Void* == null) {
        failSemaStmt(stmt, "Return type should be void");
      }
      if (stmt->expr != null) {
        semaExpr(state, stmt->expr);
        let conv = doConvert(state, stmt->expr, state->result);
        if (conv == null) {
          printType(stmt->expr->type);
          printf(" <> ");
          printType(state->result);
          failSemaStmt(stmt, "Return type mismatch");
        }
        stmt->expr = conv;
      }
    case StmtKind::COMPOUND:
      let subState = newState(state);

      for (let cur = stmt->stmt; cur != null; cur = cur->nextStmt) {
        semaStmt(&subState, cur);
      }

    case StmtKind::IF:
      let subState = newState(state);
      semaExpr(&subState, stmt->expr);

      // Add != null for let expressions.
      if (stmt->expr->kind == ExprKind::LET) {
        if (let ptrType = stmt->expr->type->kind as TypeKind::Pointer*) {
          stmt->expr = makeNullCmp(stmt->expr);
        }
      }
      checkBool(stmt->expr);

      semaStmt(&subState, stmt->init);
      if (stmt->stmt != null) {
        semaStmt(state, stmt->stmt);
      }
    case StmtKind::WHILE:
      semaExpr(state, stmt->expr);
      checkBool(stmt->expr);
      semaStmt(state, stmt->stmt);
    case StmtKind::FOR:
      let subState = newState(state);
      semaStmt(&subState, stmt->init);

      // cond must be expr stmt.
      semaExpr(&subState, stmt->cond->expr);
      checkBool(stmt->cond->expr);
      semaExpr(&subState, stmt->expr);

      semaStmt(&subState, stmt->stmt);

    case StmtKind::SWITCH:
      semaSwitchStmt(state, stmt);
    case StmtKind::CASE, StmtKind::DEFAULT:
      failSemaStmt(stmt, "Case or default outside of switch");
    case StmtKind::BREAK:
      break;
  }
}
