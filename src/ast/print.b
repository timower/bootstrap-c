import ast;
import libc;

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
    case TypeKind::Bool:
      printf("bool");
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


func printLet(decl: DeclAST*, indent: i32) {
  let isExtern = false;
  let init: ExprAST* = null;

  if (let varKind = &decl->kind as DeclKind::Var*) {
    isExtern = varKind->isExtern;
    init = varKind->init;
  } else if (let constKind = &decl->kind as DeclKind::Const*) {
    init = constKind->init;
  }

  if (isExtern) {
    printf("extern ");
  }

  if (&decl->kind as DeclKind::Const* != null) {
    printf("const ");
  } else {
    printf("let ");
  }
  printToken(decl->name);

  if (decl->type != null) {
    printf(": ");
    printType(decl->type);
  }

  if (init != null) {
    printf(" =");
    if (init->location.line != decl->location.line) {
      printf("\n");
      printIndent(indent + indent_width * 2);
    } else {
      printf(" ");
    }
    printExprPrec(init, -1, indent);
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
    case ExprKind::Let as letExpr:
      printLet(letExpr.decl, indent);

    case ExprKind::Variable as variable:
      if (tokCmpStr(variable.identifier, "NULL")) {
        printf("null");
      } else {
        printToken(variable.identifier);
      }

    case ExprKind::Int as int:
      printStr(int.token.data, int.token.end);

    // printf("%d", expr->value);
    // printToken(expr->op);
    case ExprKind::Str as str:
      printf("\"");
      printStr(str.identifier.data, str.identifier.end);
      printf("\"");
    case ExprKind::Binary as binary:
      let isComma = binary.op.kind == TokenKind::COMMA;
      let isSplit = binary.lhs->location.line != binary.rhs->location.line;
      printExprPrec(binary.lhs, curPrec, indent);
      if (!isComma) {
        if (isSplit) {
          printf("\n");
          printIndent(indent + 2 * indent_width);
        } else {
          printf(" ");
        }
      }

      printToken(binary.op);
      if (isComma && isSplit) {
        printf("\n");
        printIndent(indent + 2 * indent_width);
      } else {
        printf(" ");
      }

      let newIndent = isSplit ? indent + indent_width : indent;
      printExprPrec(binary.rhs, nextPrec, newIndent);

    case ExprKind::Index as index:
      printExprPrec(index.array, nextPrec, indent);
      printf("[");
      printExprPrec(index.index, nextPrec, indent);
      printf("]");
    case ExprKind::Call as call:
      printExprPrec(call.function, nextPrec, indent);
      printf("(");
      let split = false;
      for (let cur = call.args; cur != null; cur = cur->next) {
        if (cur->next != null && cur->location.line != cur->next->location.line) {
          split = true;
        }
      }
      for (let cur: ExprAST* = call.args; cur != null; cur = cur->next) {
        if (split) {
          printf("\n");
          printIndent(indent + indent_width * 2);
        }
        printExprPrec(cur, -1, indent);
        if (cur->next != null) {
          printf(",");
          if (!split) {
            printf(" ");
          }
        }
      }
      printf(")");
    case ExprKind::Member as member:
      printExprPrec(member.object, curPrec, indent);
      let isAs = member.op.kind == TokenKind::AS;
      if (isAs) {
        printf(" ");
      }
      printToken(member.op);
      if (isAs) {
        printf(" ");
      }
      printToken(member.identifier);
    case ExprKind::Unary as unary:
      if (unary.postfix != null) {
        printExprPrec(unary.postfix, curPrec, indent);
      }
      printToken(unary.op);
      if (unary.prefix != null) {
        printExprPrec(unary.prefix, nextPrec, indent);
      }
    case ExprKind::Sizeof as sizeofExpr:
      printf("sizeof(");
      if (sizeofExpr.typeArg != null) {
        printType(sizeofExpr.typeArg);
      } else {
        printExprPrec(sizeofExpr.expr, nextPrec, indent);
      }
      printf(")");
    case ExprKind::Conditional as cond:
      printExprPrec(cond.cond, nextPrec, indent);
      if (expr->location.line != cond.trueExpr->location.line) {
        printf("\n");
        printIndent(indent + 2 * indent_width);
      }
      printf(" ? ");
      printExprPrec(cond.trueExpr, nextPrec, indent);
      if (cond.trueExpr->location.line != cond.falseExpr->location.line) {
        printf("\n");
        printIndent(indent + 2 * indent_width);
      }
      printf(" : ");
      printExprPrec(cond.falseExpr, nextPrec, indent);
    case ExprKind::Array as array:
      printf("{");
      let hasSplit = false;
      let lastLine = expr->location.line;
      for (let elem = array.elements; elem != null; elem = elem->next) {
        if (lastLine != elem->location.line) {
          hasSplit = true;
          printf("\n");
          printIndent(indent + indent_width);
        } else {
          printf(" ");
        }

        printExprPrec(elem, nextPrec, indent);
        if (elem->next != null) {
          printf(",");
        }
        lastLine = elem->location.line;
      }
      if (hasSplit) {
        printf(",\n");
        printIndent(indent);
      }
      printf("}");
    case ExprKind::Struct as structExpr:
      if (structExpr.parent.kind != TokenKind::TOK_EOF) {
        printToken(structExpr.parent);
        printf("::");
      }
      printToken(structExpr.identifier);
      printf(" {");
      if (structExpr.fieldIndices != null) {
        printf("\n");
        for (let field = structExpr.fieldIndices; field != null; field = field->next) {
          printIndent(indent + indent_width);

          printToken(field->fieldName);
          printf(" = ");
          printExprPrec(field->value, -1, indent + indent_width);
          printf(",\n");
        }
        printIndent(indent);
      }
      printf("}");
    case ExprKind::Cast as cast:
      printExprPrec(cast.expr, curPrec, indent);
      printf(" as ");
      printType(expr->type);
    case ExprKind::Scope as scope:
      printToken(scope.parent);
      printf("::");
      printToken(scope.identifier);
    case ExprKind::Paren as paren:
      printf("(");
      printExprPrec(paren.expr, -1, indent + indent_width);
      printf(")");
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


func printIfStmt(stmt: StmtAST*, indent: i32) {
  printIndent(indent);
  while (stmt != null) {
    if (let ifStmt = &stmt->kind as StmtKind::If*) {
      printf("if (");
      printExprIndent(ifStmt->cond, indent);
      printf(") ");
      printStmtIndent(ifStmt->thenStmt, indent);
      if (ifStmt->elseStmt != null) {
        printf(" else ");
        stmt = ifStmt->elseStmt;
      } else {
        stmt = null;
      }
    } else {
      printStmtIndent(stmt, indent);
      break;
    }
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
  for (let cur: StmtAST* = stmt; cur != null; cur = cur->next) {
    printStmtIndent(cur, indent + indent_width);

    if (cur->next != null) {
      let lineDiff = cur->next->location.line - cur->endLocation.line;
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
    case StmtKind::Compound as compStmt:
      // printIndent(indent);
      printf("{\n");
      printStmtList(compStmt.stmt, indent);
      printf("\n");
      printIndent(indent);
      trailing = printComments(trailing, indent + indent_width, stmt->endLocation.line);
      printf("}");
    case StmtKind::Expr as exprStmt:
      printIndent(indent);
      if (exprStmt.expr != null) {
        printExprIndent(exprStmt.expr, indent);
      }
      printf(";");
    case StmtKind::For as forStmt:
      printIndent(indent);
      printf("for (");
      printStmtIndent(forStmt.init, 0);
      if (forStmt.init->location.line != forStmt.cond->location.line) {
        printf("\n");
        printStmtIndent(forStmt.cond, indent + 5);
      } else {
        printf(" ");
        printStmtIndent(forStmt.cond, 0);
      }
      if (forStmt.cond->location.line != forStmt.update->location.line) {
        printf("\n");
        printIndent(indent + 5);
      } else {
        printf(" ");
      }
      printExprIndent(forStmt.update, indent);
      printf(") ");
      printStmtIndent(forStmt.body, indent);
    case StmtKind::If:
      printIfStmt(stmt, indent);
    case StmtKind::Return as retStmt:
      printIndent(indent);
      printf("return");
      if (retStmt.expr != null) {
        printf(" ");
        printExprIndent(retStmt.expr, indent);
      }
      printf(";");

    case StmtKind::Switch as switchStmt:
      printIndent(indent);
      printf("switch (");
      printExprIndent(switchStmt.expr, indent);
      printf(") {\n");
      printStmtList(switchStmt.body, indent);
      printf("\n");
      printIndent(indent);
      printf("}");
    case StmtKind::Case as caseStmt:
      printIndent(indent);
      printf("case ");
      printExprIndent(caseStmt.expr, 5);
      printf(":\n");
      printStmtList(caseStmt.body, indent);
    case StmtKind::Default as defaultStmt:
      printIndent(indent);
      printf("default:\n");
      printStmtList(defaultStmt.body, indent);

    case StmtKind::Break:
      printIndent(indent);
      printf("break;");

    case StmtKind::While as whileStmt:
      printIndent(indent);
      printf("while (");
      printExprIndent(whileStmt.cond, indent);
      printf(") ");
      printStmtIndent(whileStmt.body, indent);
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
  printf(" {");
  let fields = (&decl->kind as DeclKind::Struct*)->fields;
  if (fields != null) {
    printf("\n");
  }
  for (let field = fields; field != null;
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
  if (fields != null) {
    printIndent(indent);
  }
  printf("}");
  return trailing;
}

func printDeclIndent(decl: DeclAST*, indent: i32) {
  let trailing = printComments(decl->comments, indent, decl->location.line);

  switch (decl->kind) {
    case DeclKind::Struct as structKind:
      printType(decl->type);

      trailing = printStructBody(decl, indent, trailing);
      printf(";");
    case DeclKind::Enum as enumKind:
      printType(decl->type);
      printf(" {\n");
      for (let field = enumKind.fields; field != null;
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
    case DeclKind::Union as unionKind:
      printType(decl->type);
      printf(" {");

      if (unionKind.subTypes != null) {
        printf("\n");
      }

      for (let subType = unionKind.subTypes; subType != null;
           subType = subType->next) {
        let comments = printComments(
            subType->decl->comments,
            indent + indent_width,
            subType->decl->location.line);
        printIndent(indent + indent_width);

        let structType = subType->decl->type->kind as TypeKind::Struct*;
        printToken(structType->tag);
        trailing = printStructBody(
            subType->decl,
            indent + indent_width,
            trailing);

        if (subType->next == null) {
          printf("\n");
        } else {
          let lineDiff =
              subType->next->decl->location.line - subType->decl->endLocation.line;
          if (lineDiff > 1) {
            printf("\n\n");
          } else {
            printf("\n");
          }
        }
      }

      trailing = printComments(
          trailing,
          indent + indent_width,
          decl->endLocation.line);
      printf("};");
    case DeclKind::EnumField:
      printToken(decl->name);
    case DeclKind::Var:
      printLet(decl, indent);
      printf(";");
    case DeclKind::Const:
      printLet(decl, indent);
      printf(";");
    case DeclKind::Func as funcKind:
      if (funcKind.isExtern) {
        printf("extern ");
      }
      printf("func ");
      printToken(decl->name);
      printf("(");

      let fnType = decl->type->kind as TypeKind::Func*;
      let isVarargs = fnType->isVarargs;
      let split = false;

      for (let field: DeclAST* = funcKind.fields; field != null;
           field = field->next) {
        if (field->next != null
            && field->location.line != field->next->location.line) {
          split = true;
        }
      }
      for (let field: DeclAST* = funcKind.fields; field != null;
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
      if (fnType->result->kind as TypeKind::Void* == null) {
        printf(" -> ");
        printType(fnType->result);
      }

      if (funcKind.body != null) {
        printf(" ");
        printStmt(funcKind.body);
      } else {
        printf(";");
      }
    case DeclKind::Import as importKind:
      printf("import ");
      printExpr(importKind.path);
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

func allowNoNewline(decl: DeclAST*, declNext: DeclAST*) -> bool {
  // Hack that relies on internal representation of union kind tags.
  let kind1: i32 = 0;
  memcpy(&kind1, &decl->kind, sizeof(kind1));
  let kind2: i32 = 0;
  memcpy(&kind2, &declNext->kind, sizeof(kind2));

  if (kind1 != kind2) {
    return false;
  }

  if (&decl->kind as DeclKind::Import* != null) {
    return true;
  }

  if (let funcKind = &decl->kind as DeclKind::Func*) {
    return funcKind->isExtern;
  }

  return false;
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

      if (lineDiff == 1 && allowNoNewline(decl, decl->next)) {
        newlines = 1;
      }
    } else {
      newlines = 1;
    }

    for (let i = 0; i < newlines; i++) {
      putchar(10);
    }
  }
}
