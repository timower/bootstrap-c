import ast;

let indent_width = 2;

func printStr(start: i8*, end: i8*) {
  for (let c = start; c != end; c++) {
    putchar(*c as i32);
  }
}

func printToken(token: Token) {
  if (token.kind != TokenKind::IDENTIFIER) {
    printf("%s", tokens[(token.kind as i32)]);
    return;
  }
  printStr(token.data, token.end);
}

func printType(type: Type*) {
  if (type == null) {
    printf("nullType ");
    return;
  }

  if (type->isConst) {
    printf("const ");
  }

  switch (type->kind) {
    case TypeKind::Int as int:
      if (int.isSigned) {
        printf("i%d", int.size);
      } else {
        printf("u%d", int.size);
      }
    case TypeKind::Void:
      printf("void");
    case TypeKind::Pointer as ptr:
      printType(ptr.pointee);
      printf("*");
    case TypeKind::Array as array:
      printType(array.element);
      if (array.size < 0) {
        printf("[]");
      } else {
        printf("[%d]", array.size);
      }
    case TypeKind::Struct as s:
      printf("struct ");
      printStr(s.tag.data, s.tag.end);
    case TypeKind::Func as fn:
      printf("(");
      for (let arg = fn.args; arg != null; arg = arg->next) {
        printType(arg);
        if (arg->next != null) {
          printf(",");
        }
      }
      if (fn.isVarargs) {
        printf("  ...");
      }
      printf(") -> ");
      printType(fn.result);
    case TypeKind::Enum as e:
      printf("enum ");
      printStr(e.tag.data, e.tag.end);
    case TypeKind::Union as un:
      printf("union ");
      printStr(un.tag.data, un.tag.end);

    case TypeKind::Tag as tag:
      if (tag.parent.kind != TokenKind::TOK_EOF) {
        printStr(tag.parent.data, tag.parent.end);
        printf("::");
      }
      printStr(tag.tag.data, tag.tag.end);
  }
}

func printIndent(indent: i32) {
  for (let i = 0; i < indent; i++) {
    printf(" ");
  }
}

func printExprPrec(expr: ExprAST*, parentPrec: i32, indent: i32);

func printLet(decl: DeclAST*, indent: i32) {
  printf("let ");
  printToken(decl->name);

  if (decl->type != null) {
    printf(": ");
    printType(decl->type);
  }

  if (decl->init != null) {
    printf(" =");
    if (decl->init->location.line != decl->location.line) {
      printf("\n");
      printIndent(indent + indent_width * 2);
    } else {
      printf(" ");
    }
    printExprPrec(decl->init, -1, indent);
  }
}

func printExprPrec(expr: ExprAST*, parentPrec: i32, indent: i32) {
  if (expr == null) {
    printf("ERROR: null expr");
    return;
  }

  let curPrec = getExprPrecedence(expr);
  if (curPrec < parentPrec) {
    printf("(");
  }

  let nextPrec = curPrec + 1;

  switch (expr->kind) {
    case ExprKind::LET:
      printLet(expr->decl, indent);

    case ExprKind::VARIABLE:
      if (tokCmpStr(expr->identifier, "NULL")) {
        printf("null");
      } else {
        printToken(expr->identifier);
      }

    case ExprKind::INT:
      printStr(expr->op.data, expr->op.end);

    // printf("%d", expr->value);
    // printToken(expr->op);
    case ExprKind::STR:
      printf("\"");
      printStr(expr->identifier.data, expr->identifier.end);
      printf("\"");
    case ExprKind::BINARY:
      let isComma = expr->op.kind == TokenKind::COMMA;
      let isSplit = expr->lhs->location.line != expr->rhs->location.line;
      printExprPrec(expr->lhs, curPrec, indent);
      if (!isComma) {
        if (isSplit) {
          printf("\n");
          printIndent(indent + 2 * indent_width);
        } else {
          printf(" ");
        }
      }

      printToken(expr->op);
      if (isComma && isSplit) {
        printf("\n");
        printIndent(indent + 2 * indent_width);
      } else {
        printf(" ");
      }

      let newIndent = isSplit ? indent + indent_width : indent;
      printExprPrec(expr->rhs, nextPrec, newIndent);

    case ExprKind::INDEX:
      printExprPrec(expr->lhs, nextPrec, indent);
      printf("[");
      printExprPrec(expr->rhs, nextPrec, indent);
      printf("]");
    case ExprKind::CALL:
      printExprPrec(expr->lhs, nextPrec, indent);
      printf("(");
      let split = 0;
      for (let cur = expr->rhs; cur != null; cur = cur->rhs) {
        if (cur->rhs != null && cur->location.line != cur->rhs->location.line) {
          split = 1;
        }
      }
      for (let cur: ExprAST* = expr->rhs; cur != null; cur = cur->rhs) {
        if (split) {
          printf("\n");
          printIndent(indent + indent_width * 2);
        }
        printExprPrec(cur->lhs, -1, indent);
        if (cur->rhs != null) {
          printf(",");
          if (!split) {
            printf(" ");
          }
        }
      }
      printf(")");
    case ExprKind::MEMBER:
      printExprPrec(expr->lhs, curPrec, indent);
      let isAs = expr->op.kind == TokenKind::AS;
      if (isAs) {
        printf(" ");
      }
      printToken(expr->op);
      if (isAs) {
        printf(" ");
      }
      printToken(expr->identifier);
    case ExprKind::UNARY:
      if (expr->lhs != null) {
        printExprPrec(expr->lhs, curPrec, indent);
      }
      printToken(expr->op);
      if (expr->rhs != null) {
        printExprPrec(expr->rhs, nextPrec, indent);
      }
    case ExprKind::SIZEOF:
      printf("sizeof(");
      if (expr->sizeofArg != null) {
        printType(expr->sizeofArg);
      } else {
        printExprPrec(expr->rhs, nextPrec, indent);
      }
      printf(")");
    case ExprKind::CONDITIONAL:
      printExprPrec(expr->cond, nextPrec, indent);
      if (expr->location.line != expr->lhs->location.line) {
        printf("\n");
        printIndent(indent + 2 * indent_width);
      }
      printf(" ? ");
      printExprPrec(expr->lhs, nextPrec, indent);
      if (expr->lhs->location.line != expr->rhs->location.line) {
        printf("\n");
        printIndent(indent + 2 * indent_width);
      }
      printf(" : ");
      printExprPrec(expr->rhs, nextPrec, indent);
    case ExprKind::ARRAY:
      printf("{");
      let hasSplit = 0;
      for (; expr != null; expr = expr->rhs) {
        if (expr->rhs != null
            && expr->rhs->location.line != expr->location.line) {
          printf("\n");
          printIndent(indent + indent_width);
          hasSplit = 1;
        } else {
          printf(" ");
        }

        printExprPrec(expr->lhs, nextPrec, indent);
        printf(",");
      }
      if (hasSplit) {
        printf("\n");
        printIndent(indent);
      }
      printf("}");
    case ExprKind::STRUCT:
      if (expr->parent.kind != TokenKind::TOK_EOF) {
        printToken(expr->parent);
        printf("::");
      }
      printToken(expr->identifier);
      printf(" {");
      if (expr->rhs != null) {
        printf("\n");
        for (let field = expr->rhs; field != null; field = field->rhs) {
          printIndent(indent + indent_width);
          printToken(field->identifier);
          printf(" = ");
          printExprPrec(field->lhs, -1, indent + indent_width);
          printf(",\n");
        }
        printIndent(indent);
      }
      printf("}");
    case ExprKind::CAST:
      printExprPrec(expr->lhs, curPrec, indent);
      printf(" as ");
      printType(expr->type);
    case ExprKind::SCOPE:
      printToken(expr->parent);
      printf("::");
      printToken(expr->identifier);
    case ExprKind::PAREN:
      printf("(");
      printExprPrec(expr->lhs, -1, indent + indent_width);
      printf(")");
    case ExprKind::ARG_LIST:
      break;
  }

  if (curPrec < parentPrec) {
    printf(")");
  }
}

func printExpr(expr: ExprAST*) {
  printExprPrec(expr, -1, 0);
}

func printExprIndent(expr: ExprAST*, indent: i32) {
  printExprPrec(expr, -1, indent);
}

func printDeclIndent(decl: DeclAST*, indent: i32);

func printStmtIndent(stmt: StmtAST*, indent: i32);

func printIfStmt(stmt: StmtAST*, indent: i32) {
  printIndent(indent);
  for (; stmt != null && stmt->kind == StmtKind::IF; stmt = stmt->stmt) {
    printf("if (");
    printExprIndent(stmt->expr, indent);
    printf(") ");
    printStmtIndent(stmt->init, indent);
    if (stmt->stmt != null) {
      printf(" else ");
    }
  }

  if (stmt != null) {
    printStmtIndent(stmt, indent);
  }
}

func printComments(comment: Comment*, indent: i32, line: i32) -> Comment* {
  for (; comment != null && (line == 0 || comment->location.line < line);
       comment = comment->next) {
    printIndent(indent);
    printStr(comment->value.data, comment->value.end);

    if (line != 0 || comment->next != null) {
      printf("\n");
    }
  }
  return comment;
}

func printStmtList(stmt: StmtAST*, indent: i32) {
  for (let cur: StmtAST* = stmt; cur != null; cur = cur->nextStmt) {
    printStmtIndent(cur, indent + indent_width);

    if (cur->nextStmt != null) {
      let lineDiff = cur->nextStmt->location.line - cur->endLocation.line;
      if (lineDiff > 1) {
        printf("\n\n");
      } else {
        printf("\n");
      }
    }
  }
}

func printStmtIndent(stmt: StmtAST*, indent: i32) {
  let trailing = printComments(stmt->comments, indent, stmt->location.line);

  switch (stmt->kind) {
    case StmtKind::COMPOUND:
      // printIndent(indent);
      printf("{\n");
      printStmtList(stmt->stmt, indent);
      printf("\n");
      printIndent(indent);
      trailing = printComments(trailing, indent + indent_width, stmt->endLocation.line);
      printf("}");
    case StmtKind::EXPR:
      printIndent(indent);
      if (stmt->expr != null) {
        printExprIndent(stmt->expr, indent);
      }
      printf(";");
    case StmtKind::FOR:
      printIndent(indent);
      printf("for (");
      printStmtIndent(stmt->init, 0);
      if (stmt->init->location.line != stmt->cond->location.line) {
        printf("\n");
        printStmtIndent(stmt->cond, indent + 5);
      } else {
        printf(" ");
        printStmtIndent(stmt->cond, 0);
      }
      if (stmt->cond->location.line != stmt->expr->location.line) {
        printf("\n");
        printIndent(indent + 5);
      } else {
        printf(" ");
      }
      printExprIndent(stmt->expr, indent);
      printf(") ");
      printStmtIndent(stmt->stmt, indent);
    case StmtKind::IF:
      printIfStmt(stmt, indent);
    case StmtKind::RETURN:
      printIndent(indent);
      printf("return");
      if (stmt->expr != null) {
        printf(" ");
        printExprIndent(stmt->expr, indent);
      }
      printf(";");

    case StmtKind::SWITCH:
      printIndent(indent);
      printf("switch (");
      printExprIndent(stmt->expr, indent);
      printf(") {\n");
      printStmtList(stmt->stmt, indent);
      printf("\n");
      printIndent(indent);
      printf("}");
    case StmtKind::CASE:
      printIndent(indent);
      printf("case ");
      printExprIndent(stmt->expr, 5);
      printf(":\n");
      printStmtList(stmt->stmt, indent);
    case StmtKind::DEFAULT:
      printIndent(indent);
      printf("default:\n");
      printStmtList(stmt->stmt, indent);

    case StmtKind::BREAK:
      printIndent(indent);
      printf("break;");

    case StmtKind::WHILE:
      printIndent(indent);
      printf("while (");
      printExprIndent(stmt->expr, indent);
      printf(") ");
      printStmtIndent(stmt->stmt, indent);
  }

  printComments(trailing, indent, 0);
}

func printStmt(stmt: StmtAST*) {
  printStmtIndent(stmt, 0);
}

func printDeclNewlines(field: DeclAST*) {
  if (field->next == null) {
    printf("\n");
    return;
  }

  // TODO: take comments into account.
  let lineDiff = field->next->location.line - field->location.line;
  if (lineDiff > 1) {
    printf("\n\n");
  } else {
    printf("\n");
  }
}

func printStructBody(
    decl: DeclAST*,
    indent: i32,
    trailing: Comment*
) -> Comment* {
  printf(" {\n");
  for (let field = decl->fields; field != null;
       field = field->next) {
    let comments = printComments(
        field->comments,
        indent + indent_width,
        field->location.line);
    printIndent(indent + indent_width);

    // printf("%d: ", field->location.line);
    printToken(field->name);
    printf(": ");
    printType(field->type);
    printf(";");

    printComments(comments, indent + indent_width, 0);

    printDeclNewlines(field);
  }
  trailing = printComments(
      trailing,
      indent + indent_width,
      decl->endLocation.line);
  printIndent(indent);
  printf("}");
  return trailing;
}

func printDeclIndent(decl: DeclAST*, indent: i32) {
  let trailing = printComments(decl->comments, indent, decl->location.line);

  switch (decl->kind) {
    case DeclKind::STRUCT:
      printType(decl->type);

      trailing = printStructBody(decl, indent, trailing);
      printf(";");
    case DeclKind::ENUM:
      printType(decl->type);
      printf(" {\n");
      for (let field = decl->fields; field != null;
           field = field->next) {
        let comments = printComments(
            field->comments,
            indent + indent_width,
            field->location.line);
        printIndent(indent + indent_width);

        // printf("%d: ", field->location.line);
        printToken(field->name);
        printf(",");
        printComments(comments, indent + indent_width, 0);
        printDeclNewlines(field);
      }
      trailing = printComments(trailing, indent + indent_width, decl->endLocation.line);
      printf("};");
    case DeclKind::UNION:
      printType(decl->type);
      printf(" {\n");

      for (let subType = decl->subTypes; subType != null;
           subType = subType->next) {
        printIndent(indent + indent_width);
        let structType = &subType->decl->type->kind as TypeKind::Struct*;
        printToken(structType->tag);
        trailing = printStructBody(
            subType->decl,
            indent + indent_width,
            trailing);
        printf("\n");
      }

      printf("};");

    case DeclKind::ENUM_FIELD:
      printToken(decl->name);

    case DeclKind::VAR:
      printLet(decl, indent);
      printf(";");

    case DeclKind::FUNC:
      printf("func ");
      printToken(decl->name);
      printf("(");

      let fnType = &decl->type->kind as TypeKind::Func*;
      let isVarargs = fnType->isVarargs;
      let split = 0;

      for (let field: DeclAST* = decl->fields; field != null;
           field = field->next) {
        if (field->next != null
            && field->location.line != field->next->location.line) {
          split = 1;
        }
      }
      for (let field: DeclAST* = decl->fields; field != null;
           field = field->next) {
        if (split) {
          printf("\n");
          printIndent(indent + 2 * indent_width);
        }
        printToken(field->name);
        printf(": ");
        printType(field->type);
        if (field->next != null || isVarargs) {
          printf(",");
          if (!split) {
            printf(" ");
          }
        }
      }
      if (isVarargs) {
        if (split) {
          printf("\n");
          printIndent(indent + 2 * indent_width);
        }
        printf("...");
      }
      if (split) {
        printf("\n");
      }
      printf(")");
      if (&fnType->result->kind as TypeKind::Void* == null) {
        printf(" -> ");
        printType(fnType->result);
      }

      if (decl->body != null) {
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

  if (trailing != null) {
    printf(" ");
    printComments(trailing, indent, 0);
  }
}

func printDecl(decl: DeclAST*) {
  printDeclIndent(decl, 0);
}

func printTopLevel(decls: DeclAST*) {
  for (let decl = decls; decl != null; decl = decl->next) {
    printDecl(decl);

    let newlines = 0;
    if (decl->next != null) {
      let lineDiff = decl->next->location.line - decl->endLocation.line;
      if (lineDiff > 2) {
        newlines = 3;
      } else {
        newlines = 2;
      }

      if (decl->next->kind == DeclKind::IMPORT
          && decl->kind == DeclKind::IMPORT) {
        newlines--;
      }
    } else {
      newlines = 1;
    }

    for (let i = 0; i < newlines; i++) {
      putchar(10);
    }
  }
}
