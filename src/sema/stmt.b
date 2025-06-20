import state;

import expr;


func getFieldCount(decl: DeclAST*) -> i32 {
  let count = 0;

  switch (decl->kind) {
    case DeclKind::Enum as enumKind:
      for (let field = enumKind.fields; field != null; field = field->next) {
        count++;
      }
    case DeclKind::Union as unionKind:
      for (let tag = unionKind.subTypes; tag != null; tag = tag->next) {
        count++;
      }
    default:
      break;
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
      let tagDecl = findTypeIdx((&unionDecl->kind as DeclKind::Union*)->subTypes, tagName, &tagIdx);
      if (tagDecl == null) {
        failSemaExpr(expr, "Unkown tag in union");
      }

      expr->type = unionDecl->type;
      expr->value = tagIdx;

      // Make a new variable declaration.
      let varDecl = newDecl(DeclKind::Var {});
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
              (&decl->kind as DeclKind::Union*)->subTypes,
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
  let switchStmt = &stmt->kind as StmtKind::Switch*;
  semaExpr(state, switchStmt->expr);
  let switchType = switchStmt->expr->type;

  let exhaustive =
      &switchType->kind as TypeKind::Enum* != null
      || &switchType->kind as TypeKind::Union* != null;

  if (&switchType->kind as TypeKind::Int* == null && !exhaustive) {
    printType(switchStmt->expr->type);
    failSemaExpr(switchStmt->expr, "Switch expr must be integer, enum or union");
  }

  let fieldBitSet = 0;

  for (let caseStmt = switchStmt->body; caseStmt != null; caseStmt = caseStmt->next) {
    let subState = newState(state);

    switch (caseStmt->kind) {
      case StmtKind::Case as caseKind:
        semaCaseExpr(&subState, switchType, caseKind.expr);

        if (exhaustive) {
          fieldBitSet |= getFieldBitset(&subState, caseKind.expr);
        }

        // Process statements in this case
        for (let cur = caseKind.body; cur != null; cur = cur->next) {
          semaStmt(&subState, cur);
        }
      case StmtKind::Default as defaultKind:
        fieldBitSet = -1;

        // Process statements in this default case
        for (let cur = defaultKind.body; cur != null; cur = cur->next) {
          semaStmt(&subState, cur);
        }
      default:
        failSemaStmt(caseStmt, "Unknown switch case statement");
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

  cmpExpr->type = getBool();

  return cmpExpr;
}

func semaStmt(state: SemaState*, stmt: StmtAST*) {
  switch (stmt->kind) {
    case StmtKind::Expr as exprStmt:
      if (exprStmt.expr != null) {
        semaExpr(state, exprStmt.expr);
      }

    case StmtKind::Return as retStmt:
      if (retStmt.expr == null && &state->result->kind as TypeKind::Void* == null) {
        failSemaStmt(stmt, "Return type should be void");
      }
      if (retStmt.expr != null) {
        semaExpr(state, retStmt.expr);
        let conv = doConvert(state, retStmt.expr, state->result);
        if (conv == null) {
          failSemaStmt(stmt, "Return type mismatch");
        }
        retStmt.expr = conv;
      }
    case StmtKind::Compound as compStmt:
      let subState = newState(state);

      for (let cur = compStmt.stmt; cur != null; cur = cur->next) {
        semaStmt(&subState, cur);
      }

    case StmtKind::If as ifStmt:
      let subState = newState(state);
      semaExpr(&subState, ifStmt.cond);

      // Add != null for let expressions.
      if (ifStmt.cond->kind == ExprKind::LET) {
        if (let ptrType = &ifStmt.cond->type->kind as TypeKind::Pointer*) {
          ifStmt.cond = makeNullCmp(ifStmt.cond);
        }
      }
      checkBool(ifStmt.cond);

      semaStmt(&subState, ifStmt.thenStmt);
      if (ifStmt.elseStmt != null) {
        semaStmt(state, ifStmt.elseStmt);
      }
    case StmtKind::While as whileStmt:
      semaExpr(state, whileStmt.cond);
      checkBool(whileStmt.cond);
      semaStmt(state, whileStmt.body);
    case StmtKind::For as forStmt:
      let subState = newState(state);
      semaStmt(&subState, forStmt.init);

      // cond must be expr stmt.
      let condExpr = (&forStmt.cond->kind as StmtKind::Expr*)->expr;
      semaExpr(&subState, condExpr);
      checkBool(condExpr);
      semaExpr(&subState, forStmt.update);

      semaStmt(&subState, forStmt.body);

    case StmtKind::Switch as switchStmt:
      semaSwitchStmt(state, stmt);
    case StmtKind::Case:
      failSemaStmt(stmt, "Case outside of switch");
    case StmtKind::Default:
      failSemaStmt(stmt, "Default outside of switch");
    case StmtKind::Break:
      break;
  }
}
