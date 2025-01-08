import ast;

func printStr(start : i8 *, end : i8 *) {
  for (let c = start; c != end; c++) {
    putchar(*c as i32);
  }
}

func printToken(token : Token) {
  if (token.kind != TokenKind::IDENTIFIER) {
    printf("%s", tokens[token.kind as i32]);
    return;
  }
  printStr(token.data, token.end);
}

func printType(type : Type *) {
  if (type == NULL) {
    printf("nullType ");
    return;
  }

  if (type->isConst) {
    printf("const ");
  }
  switch (type->kind) {
  case TypeKind::INT:
    if (type->isSigned) {
      printf("i%d", type->size);
    } else {
      printf("u%d", type->size);
    }
  case TypeKind::VOID:
    printf("void");
  case TypeKind::POINTER:
    printType(type->arg);
    printf("*");
  case TypeKind::ARRAY:
    printType(type->arg);
    if (type->size < 0) {
      printf("[]");
    } else {
      printf("[%d]", type->size);
    }
  case TypeKind::STRUCT:
    printf("struct ");
    printStr(type->tag.data, type->tag.end);
  case TypeKind::FUNC:
    printf("(");
    for (let arg : Type * = type->arg; arg != NULL; arg = arg->argNext) {
      printType(arg);
      if (arg->argNext != NULL) {
        printf(",");
      }
    }
    if (type->isVarargs) {
      printf("  ...");
    }
    printf(") -> ");
    printType(type->result);
  case TypeKind::ENUM:
    printf("enum ");
    printStr(type->tag.data, type->tag.end);
  case TypeKind::TAG:
    printStr(type->tag.data, type->tag.end);
  }
}

func printExprPrec(expr : ExprAST *, parentPrec : i32) {
  if (expr == NULL) {
    printf("ERROR: null expr");
    return;
  }

  let curPrec = getExprPrecedence(expr);
  if (curPrec < parentPrec) {
    printf("(");
  }

  let nextPrec = curPrec + 1;

  switch (expr->kind) {
  case ExprKind::VARIABLE:
    printToken(expr->identifier);
  case ExprKind::INT:
    printf("%d", expr->value);
  case ExprKind::STR:
    printf("\"");
    printStr(expr->identifier.data, expr->identifier.end);
    printf("\"");
  case ExprKind::BINARY:
    printExprPrec(expr->lhs, curPrec);
    if (expr->op.kind != TokenKind::COMMA) {
      printf(" ");
    }
    printToken(expr->op);
    printf(" ");
    printExprPrec(expr->rhs, nextPrec);

  case ExprKind::INDEX:
    printExprPrec(expr->lhs, nextPrec);
    printf("[");
    printExprPrec(expr->rhs, nextPrec);
    printf("]");
  case ExprKind::CALL:
    printExprPrec(expr->lhs, nextPrec);
    printf("(");
    for (let cur : ExprAST * = expr->rhs; cur != NULL; cur = cur->rhs) {
      printExprPrec(cur->lhs, -1);
      if (cur->rhs != NULL) {
        printf(", ");
      }
    }
    printf(")");
  case ExprKind::MEMBER:
    printExprPrec(expr->lhs, curPrec);
    printToken(expr->op);
    printToken(expr->identifier);
  case ExprKind::UNARY:
    if (expr->lhs != NULL) {
      printExprPrec(expr->lhs, nextPrec);
    }
    printToken(expr->op);
    if (expr->rhs != NULL) {
      printExprPrec(expr->rhs, nextPrec);
    }
  case ExprKind::SIZEOF:
    printf("sizeof(");
    if (expr->sizeofArg != NULL) {
      printType(expr->sizeofArg);
    } else {
      printExprPrec(expr->rhs, nextPrec);
    }
    printf(")");
  case ExprKind::CONDITIONAL:
    printExprPrec(expr->cond, nextPrec);
    printf(" ? ");
    printExprPrec(expr->lhs, nextPrec);
    printf(" : ");
    printExprPrec(expr->rhs, nextPrec);
  case ExprKind::ARRAY:
    printf("{");
    for (; expr != NULL; expr = expr->rhs) {
      printExprPrec(expr->lhs, nextPrec);
      printf(", ");
    }
    printf("}");
  case ExprKind::STRUCT:
    printToken(expr->identifier);
    printf("{ ");
    for (let field = expr->rhs; field != NULL; field = field->rhs) {
      printToken(field->identifier);
      printf(" = ");
      printExprPrec(field->lhs, -1);
      printf(", ");
    }
    printf("}");
  case ExprKind::CAST:
    printExprPrec(expr->lhs, curPrec);
    printf(" as ");
    printType(expr->type);
  case ExprKind::SCOPE:
    printToken(expr->parent);
    printf("::");
    printToken(expr->identifier);
  case ExprKind::PAREN:
    printf("(");
    printExprPrec(expr->lhs, -1);
    printf(")");
  case ExprKind::ARG_LIST:
    break;
  }

  if (curPrec < parentPrec) {
    printf(")");
  }
}

func printExpr(expr : ExprAST *) { printExprPrec(expr, -1); }

func printDecl(decl : DeclAST *);

func printIndent(indent : i32) {
  for (let i = 0; i < indent; i++) {
    printf("  ");
  }
}

func printStmtIndent(stmt : StmtAST *, indent : i32);

func printIfStmt(stmt : StmtAST *, indent : i32) {
  printIndent(indent);
  for (; stmt != NULL && stmt->kind == StmtKind::IF; stmt = stmt->stmt) {
    printf("if (");
    printExpr(stmt->expr);
    printf(") ");
    printStmtIndent(stmt->init, indent);
    if (stmt->stmt != NULL) {
      printf(" else ");
    }
  }

  if (stmt != NULL) {
    printStmtIndent(stmt, indent);
  }
}

func printStmtIndent(stmt : StmtAST *, indent : i32) {

  switch (stmt->kind) {
  case StmtKind::DECL:
    printIndent(indent);
    printDecl(stmt->decl);
  case StmtKind::COMPOUND:
    // printIndent(indent);
    printf("{\n");
    for (let cur : StmtAST * = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmtIndent(cur, indent + 1);
      printf("\n");
    }
    printIndent(indent);
    printf("}");
  case StmtKind::EXPR:
    printIndent(indent);
    if (stmt->expr != NULL) {
      printExpr(stmt->expr);
    }
    printf(";");
  case StmtKind::FOR:
    printIndent(indent);
    printf("for (");
    printStmtIndent(stmt->init, 0);
    printf(" ");
    printStmtIndent(stmt->cond, 0);
    printf(" ");
    printExpr(stmt->expr);
    printf(") ");
    printStmtIndent(stmt->stmt, indent);
  case StmtKind::IF:
    printIfStmt(stmt, indent);
  case StmtKind::RETURN:
    printIndent(indent);
    printf("return");
    if (stmt->expr != NULL) {
      printf(" ");
      printExpr(stmt->expr);
    }
    printf(";");
  case StmtKind::SWITCH:
    printIndent(indent);
    printf("switch (");
    printExpr(stmt->expr);
    printf(") {\n");
    for (let cur : StmtAST * = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmtIndent(cur, indent + 1);
      if (cur->nextStmt != NULL) {
        printf("\n");
      }
    }
    printIndent(indent);
    printf("}");
  case StmtKind::DEFAULT:
    printIndent(indent);
    printf("default:\n");
    for (let cur : StmtAST * = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmtIndent(cur, indent + 1);
      printf("\n");
    }
  case StmtKind::BREAK:
    printIndent(indent);
    printf("break;");
  case StmtKind::CASE:
    printIndent(indent);
    printf("case ");
    printExpr(stmt->expr);
    printf(":\n");
    for (let cur : StmtAST * = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      printStmtIndent(cur, indent + 1);
      printf("\n");
    }
  case StmtKind::WHILE:
    printIndent(indent);
    printf("while (");
    printExpr(stmt->expr);
    printf(") ");
    printStmtIndent(stmt->stmt, indent);
  }
}

func printStmt(stmt : StmtAST *) { printStmtIndent(stmt, 0); }

func printDecl(decl : DeclAST *) {
  switch (decl->kind) {
  case DeclKind::STRUCT:
    printType(decl->type);
    printf(" {\n");
    for (let field : DeclAST * = decl->fields; field != NULL;
         field = field->next) {
      printf("  ");
      printToken(field->name);
      printf(": ");
      printType(field->type);
      printf(";\n");
    }
    printf("};");
  case DeclKind::ENUM:
    printType(decl->type);
    printf(" {\n");
    for (let field : DeclAST * = decl->fields; field != NULL;
         field = field->next) {
      printf("  ");
      printToken(field->name);
      printf(",");
      printf("\n");
    }
    printf("};");
  case DeclKind::ENUM_FIELD:
    printToken(decl->name);
  case DeclKind::VAR:
    printf("let ");
    printToken(decl->name);

    if (decl->type != NULL) {
      printf(": ");
      printType(decl->type);
    }

    if (decl->init != NULL) {
      printf(" = ");
      printExpr(decl->init);
    }
    printf(";");
  case DeclKind::FUNC:
    printf("func ");
    printToken(decl->name);
    printf("(");
    for (let field : DeclAST * = decl->fields; field != NULL;
         field = field->next) {
      printToken(field->name);
      printf(": ");
      printType(field->type);
      if (field->next != NULL) {
        printf(", ");
      }
    }
    printf(")");
    if (decl->type->result->kind != TypeKind::VOID) {
      printf(" -> ");
      printType(decl->type->result);
    }

    if (decl->body != NULL) {
      printf(" ");
      printStmt(decl->body);
    } else {
      printf(";");
    }
  case DeclKind::IMPORT:
    printf("import ");
    printToken(decl->name);
    printf(";");
  }
}

func printTopLevel(decls : DeclAST *) {
  for (let decl = decls; decl != NULL; decl = decl->next) {
    printDecl(decl);
    printf("\n\n");
  }
}
