import ast;
import libc;

let indent_width = 2;

let printFile: void* = null;

func printStr(data: [i8]) {
  fprintf(printFile, "%.*s", data.len as i32, &data[0]);
}

func printRawToken(token: Token) {
  printStr(token.data);
}

func printToken(token: Token) {
  if (token.kind != TokenKind::IDENTIFIER) {
    fprintf(printFile, "%s", tokens[(token.kind as i32)]);
    return;
  }
  printRawToken(token);
}

func printType(type: Type*) {
  let fnType = printTypeSub(type);
  if (fnType == null) {
    return;
  }

  fprintf(printFile, "(");
  for (let arg = fnType->args; arg != null; arg = arg->next) {
    printType(arg);
    if (arg->next != null || fnType->isVarargs) {
      fprintf(printFile, ", ");
    }
  }
  if (fnType->isVarargs) {
    fprintf(printFile, "...");
  }
  fprintf(printFile, ")");
  if (fnType->result->kind as TypeKind::Void* == null) {
    fprintf(printFile, " -> ");
    printType(fnType->result);
  }
}

func printTypeSub(type: Type*) -> TypeKind::Func* {
  if (type == null) {
    unreachable("nullType");
    return null;
  }

  let res: TypeKind::Func* = null;
  if (type->isConst) {
    fprintf(printFile, "const ");
  }

  switch (type->kind) {
    case TypeKind::Int as int:
      if (int.isPtr) {
        fprintf(printFile, "%cptr", int.isSigned ? 'i' : 'u');
      } else if (int.isSigned) {
        fprintf(printFile, "i%d", int.size);
      } else {
        fprintf(printFile, "u%d", int.size);
      }
    case TypeKind::Void:
      fprintf(printFile, "void");
    case TypeKind::Bool:
      fprintf(printFile, "bool");
    case TypeKind::Pointer as ptr:
      res = printTypeSub(ptr.pointee);
      fprintf(printFile, "*");
    case TypeKind::Array as array:
      res = printTypeSub(array.element);
      if (array.size < 0) {
        fprintf(printFile, "[]");
      } else {
        fprintf(printFile, "[%d]", array.size);
      }
    case TypeKind::Struct as s:
      fprintf(printFile, "struct ");
      printToken(s.tag);
    case TypeKind::Func as fn:
      fprintf(printFile, "func");
      res = &fn;
    case TypeKind::Enum as e:
      fprintf(printFile, "enum ");
      printToken(e.tag);
    case TypeKind::Union as un:
      fprintf(printFile, "union ");
      printToken(un.tag);

    case TypeKind::Tag as tag:
      if (tag.parent.kind != TokenKind::TOK_EOF) {
        printToken(tag.parent);
        fprintf(printFile, "::");
      }
      printToken(tag.tag);

    case TypeKind::Typeof as typeofType:
      fprintf(printFile, "typeof(");
      printExpr(typeofType.expr);
      fprintf(printFile, ")");

    case TypeKind::Slice as s:
      fprintf(printFile, "[");
      printType(s.element);
      fprintf(printFile, "]");
  }

  return res;
}

func printIndent(indent: i32) {
  for (let i = 0; i < indent; i++) {
    fprintf(printFile, " ");
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
    fprintf(printFile, "extern ");
  }

  if (&decl->kind as DeclKind::Const* != null) {
    fprintf(printFile, "const ");
  } else {
    fprintf(printFile, "let ");
  }
  printToken(decl->name);

  if (decl->type != null) {
    fprintf(printFile, ": ");
    printType(decl->type);
  }

  if (init != null) {
    fprintf(printFile, " =");
    if (init->location->line != decl->location->line) {
      fprintf(printFile, "\n");
      printIndent(indent + indent_width * 2);
    } else {
      fprintf(printFile, " ");
    }
    printExprIndent(init, indent);
  }
}

func printExprIndent(expr: ExprAST*, indent: i32) {
  if (expr == null) {
    unreachable("ERROR: null expr");
    return;
  }

  switch (expr->kind) {
    case ExprKind::Let as letExpr:
      printLet(letExpr.decl, indent);

    case ExprKind::Variable as variable:
      printToken(variable.identifier);

    case ExprKind::Int as int:
      printRawToken(int.token);

    // fprintf(printFile, "%d", expr->value);
    // printToken(expr->op);
    case ExprKind::Str as str:
      fprintf(printFile, "\"");
      printRawToken(str.identifier);
      fprintf(printFile, "\"");
    case ExprKind::Binary as binary:
      let isComma = binary.op.kind == TokenKind::COMMA;
      let isSplit = binary.lhs->location->line != binary.rhs->location->line;
      printExprIndent(binary.lhs, indent);
      if (!isComma) {
        if (isSplit) {
          fprintf(printFile, "\n");
          printIndent(indent + 2 * indent_width);
        } else {
          fprintf(printFile, " ");
        }
      }

      printToken(binary.op);
      if (isComma && isSplit) {
        fprintf(printFile, "\n");
        printIndent(indent + 2 * indent_width);
      } else {
        fprintf(printFile, " ");
      }

      let newIndent = isSplit ? indent + indent_width : indent;
      printExprIndent(binary.rhs, newIndent);

    case ExprKind::Index as index:
      printExprIndent(index.array, indent);
      fprintf(printFile, "[");
      printExprIndent(index.index, indent);
      fprintf(printFile, "]");

    case ExprKind::SliceIndex as index:
      printExprIndent(index.slice, indent);
      fprintf(printFile, "[");
      if (index.start != null) {
        printExprIndent(index.start, indent);
      }
      fprintf(printFile, ":");
      if (index.end != null) {
        printExprIndent(index.end, indent);
      }
      fprintf(printFile, "]");

    case ExprKind::Call as call:
      printExprIndent(call.function, indent);
      fprintf(printFile, "(");
      let split = false;
      for (let cur = call.args; cur != null; cur = cur->next) {
        if (cur->next != null && cur->location->line != cur->next->location->line) {
          split = true;
        }
      }
      for (let cur: ExprAST* = call.args; cur != null; cur = cur->next) {
        if (split) {
          fprintf(printFile, "\n");
          printIndent(indent + indent_width * 2);
        }
        printExprIndent(cur, indent);
        if (cur->next != null) {
          fprintf(printFile, ",");
          if (!split) {
            fprintf(printFile, " ");
          }
        }
      }
      fprintf(printFile, ")");
    case ExprKind::GenericInstantiation as genericInst:
      printToken(genericInst.function);
      fprintf(printFile, ":[");
      for (let tp = genericInst.typeArgs; tp != null; tp = tp->next) {
        printType(tp);
        if (tp->next != null) {
          fprintf(printFile, ", ");
        }
      }
      fprintf(printFile, "]");
    case ExprKind::Member as member:
      printExprIndent(member.object, indent);
      let isAs = member.op.kind == TokenKind::AS;
      if (isAs) {
        fprintf(printFile, " ");
      }
      printToken(member.op);
      if (isAs) {
        fprintf(printFile, " ");
      }
      printToken(member.identifier);
    case ExprKind::Unary as unary:
      if (unary.postfix != null) {
        printExprIndent(unary.postfix, indent);
      }
      printToken(unary.op);
      if (unary.prefix != null) {
        printExprIndent(unary.prefix, indent);
      }
    case ExprKind::Sizeof as sizeofExpr:
      fprintf(printFile, "sizeof(");
      printType(sizeofExpr.typeArg);
      fprintf(printFile, ")");
    case ExprKind::Conditional as cond:
      printExprIndent(cond.cond, indent);
      if (expr->location->line != cond.trueExpr->location->line) {
        fprintf(printFile, "\n");
        printIndent(indent + 2 * indent_width);
      }
      fprintf(printFile, " ? ");
      printExprIndent(cond.trueExpr, indent);
      if (cond.trueExpr->location->line != cond.falseExpr->location->line) {
        fprintf(printFile, "\n");
        printIndent(indent + 2 * indent_width);
      }
      fprintf(printFile, " : ");
      printExprIndent(cond.falseExpr, indent);
    case ExprKind::Array as array:
      fprintf(printFile, "[");
      let hasSplit = false;
      let lastLine = expr->location->line;
      for (let elem = array.elements; elem != null; elem = elem->next) {
        if (lastLine != elem->location->line) {
          hasSplit = true;
          fprintf(printFile, "\n");
          printIndent(indent + indent_width);
        } else {
          fprintf(printFile, " ");
        }

        printExprIndent(elem, indent);
        if (elem->next != null) {
          fprintf(printFile, ",");
        }
        lastLine = elem->location->line;
      }
      if (hasSplit) {
        fprintf(printFile, ",\n");
        printIndent(indent);
      } else {
        fprintf(printFile, " ");
      }
      fprintf(printFile, "]");
    case ExprKind::Struct as structExpr:
      if (structExpr.parent.kind != TokenKind::TOK_EOF) {
        printToken(structExpr.parent);
        fprintf(printFile, "::");
      }
      printToken(structExpr.identifier);
      fprintf(printFile, " {");
      if (structExpr.fieldIndices != null) {
        fprintf(printFile, "\n");
        for (let field = structExpr.fieldIndices; field != null; field = field->next) {
          printIndent(indent + indent_width);

          printToken(field->fieldName);
          fprintf(printFile, " = ");
          printExprIndent(field->value, indent + indent_width);
          fprintf(printFile, ",\n");
        }
        printIndent(indent);
      }
      fprintf(printFile, "}");
    case ExprKind::Cast as cast:
      printExprIndent(cast.expr, indent);
      fprintf(printFile, " as ");
      printType(expr->type);
    case ExprKind::Scope as scope:
      printToken(scope.parent);
      fprintf(printFile, "::");
      printToken(scope.identifier);
    case ExprKind::Paren as paren:
      fprintf(printFile, "(");
      printExprIndent(paren.expr, indent + indent_width);
      fprintf(printFile, ")");
  }
}

func printExpr(expr: ExprAST*) {
  printExprIndent(expr, 0);
}

func printIfStmt(stmt: StmtAST*, indent: i32) {
  printIndent(indent);
  while (stmt != null) {
    if (let ifStmt = &stmt->kind as StmtKind::If*) {
      fprintf(printFile, "if (");
      printExprIndent(ifStmt->cond, indent);
      fprintf(printFile, ") ");
      printStmtIndent(ifStmt->thenStmt, indent, false);
      if (ifStmt->elseStmt != null) {
        fprintf(printFile, " else ");
        stmt = ifStmt->elseStmt;
      } else {
        stmt = null;
      }
    } else {
      printStmtIndent(stmt, indent, false);
      break;
    }
  }
}

func printComments(comment: Comment*, indent: i32, line: i32) -> Comment* {
  for (; comment != null && (line == 0 || comment->location->line < line);
       comment = comment->next) {
    printIndent(indent);
    printRawToken(comment->value);

    if (line != 0 || comment->next != null) {
      fprintf(printFile, "\n");
    }
  }
  return comment;
}

func printStmtList(stmt: StmtAST*, indent: i32) {
  for (let cur: StmtAST* = stmt; cur != null; cur = cur->next) {
    printStmtIndent(cur, indent, true);

    if (cur->next != null) {
      let lineDiff = cur->next->location->line - cur->endLocation->line;
      if (lineDiff > 1) {
        fprintf(printFile, "\n\n");
      } else {
        fprintf(printFile, "\n");
      }
    }
  }
}

func printStmtIndent(stmt: StmtAST*, indent: i32, breakCompound: bool) {
  let trailing = printComments(stmt->comments, indent, stmt->location->line);

  switch (stmt->kind) {
    case StmtKind::Compound as compStmt:
      if (breakCompound) {
        printIndent(indent);
      }
      fprintf(printFile, "{\n");
      printStmtList(compStmt.stmt, indent + indent_width);
      fprintf(printFile, "\n");
      trailing = printComments(trailing, indent + indent_width, stmt->endLocation->line);
      printIndent(indent);
      fprintf(printFile, "}");
    case StmtKind::Expr as exprStmt:
      printIndent(indent);
      if (exprStmt.expr != null) {
        printExprIndent(exprStmt.expr, indent);
      }
      fprintf(printFile, ";");
    case StmtKind::For as forStmt:
      printIndent(indent);
      fprintf(printFile, "for (");
      printStmtIndent(forStmt.init, 0, false);
      if (forStmt.init->location->line != forStmt.cond->location->line) {
        fprintf(printFile, "\n");
        printStmtIndent(forStmt.cond, indent + 5, false);
      } else {
        fprintf(printFile, " ");
        printStmtIndent(forStmt.cond, 0, false);
      }
      if (forStmt.cond->location->line != forStmt.update->location->line) {
        fprintf(printFile, "\n");
        printIndent(indent + 5);
      } else {
        fprintf(printFile, " ");
      }
      printExprIndent(forStmt.update, indent);
      fprintf(printFile, ") ");
      printStmtIndent(forStmt.body, indent, false);
    case StmtKind::If:
      printIfStmt(stmt, indent);
    case StmtKind::Return as retStmt:
      printIndent(indent);
      fprintf(printFile, "return");
      if (retStmt.expr != null) {
        fprintf(printFile, " ");
        printExprIndent(retStmt.expr, indent);
      }
      fprintf(printFile, ";");

    case StmtKind::Switch as switchStmt:
      printIndent(indent);
      fprintf(printFile, "switch (");
      printExprIndent(switchStmt.expr, indent);
      fprintf(printFile, ") {\n");
      printStmtList(switchStmt.body, indent + indent_width);
      fprintf(printFile, "\n");
      printIndent(indent);
      fprintf(printFile, "}");
    case StmtKind::Case as caseStmt:
      printIndent(indent);
      fprintf(printFile, "case ");
      printExprIndent(caseStmt.expr, 5);
      fprintf(printFile, ":\n");
      printStmtList(caseStmt.body, indent + indent_width);
    case StmtKind::Default as defaultStmt:
      printIndent(indent);
      fprintf(printFile, "default:\n");
      printStmtList(defaultStmt.body, indent + indent_width);

    case StmtKind::Break:
      printIndent(indent);
      fprintf(printFile, "break;");

    case StmtKind::Continue:
      printIndent(indent);
      fprintf(printFile, "continue;");

    case StmtKind::While as whileStmt:
      printIndent(indent);
      fprintf(printFile, "while (");
      printExprIndent(whileStmt.cond, indent);
      fprintf(printFile, ") ");
      printStmtIndent(whileStmt.body, indent, false);
  }

  printComments(trailing, indent, 0);
}

func printStmt(stmt: StmtAST*) {
  printStmtIndent(stmt, 0, false);
}

func printDeclNewlines(field: DeclAST*) {
  if (field->next == null) {
    fprintf(printFile, "\n");
    return;
  }

  // TODO: take comments into account.
  let lineDiff = field->next->location->line - field->location->line;
  if (lineDiff > 1) {
    fprintf(printFile, "\n\n");
  } else {
    fprintf(printFile, "\n");
  }
}

func printStructBody(
    decl: DeclAST*,
    indent: i32,
    trailing: Comment*
) -> Comment* {
  fprintf(printFile, " {");
  let fields = (&decl->kind as DeclKind::Struct*)->fields;
  if (fields != null) {
    fprintf(printFile, "\n");
  }
  for (let field = fields; field != null;
       field = field->next) {
    let comments = printComments(
        field->comments,
        indent + indent_width,
        field->location->line);
    printIndent(indent + indent_width);

    // fprintf(printFile, "%d: ", field->location.line);
    printToken(field->name);
    fprintf(printFile, ": ");
    printType(field->type);
    fprintf(printFile, ";");

    printComments(comments, indent + indent_width, 0);

    printDeclNewlines(field);
  }

  trailing = printComments(
      trailing,
      indent + indent_width,
      decl->endLocation->line);
  if (fields != null) {
    printIndent(indent);
  }
  fprintf(printFile, "}");
  return trailing;
}

func printDeclIndent(decl: DeclAST*, indent: i32) {
  let trailing = printComments(decl->comments, indent, decl->location->line);

  switch (decl->kind) {
    case DeclKind::Struct as structKind:
      printType(decl->type);

      trailing = printStructBody(decl, indent, trailing);
    case DeclKind::Enum as enumKind:
      printType(decl->type);
      fprintf(printFile, " {\n");
      for (let field = enumKind.fields; field != null;
           field = field->next) {
        let comments = printComments(
            field->comments,
            indent + indent_width,
            field->location->line);
        printIndent(indent + indent_width);

        // fprintf(printFile, "%d: ", field->location.line);
        printToken(field->name);
        fprintf(printFile, ",");
        printComments(comments, indent + indent_width, 0);
        printDeclNewlines(field);
      }
      trailing = printComments(trailing, indent + indent_width, decl->endLocation->line);
      fprintf(printFile, "}");
    case DeclKind::Union as unionKind:
      printType(decl->type);
      fprintf(printFile, " {");

      if (unionKind.subTypes != null) {
        fprintf(printFile, "\n");
      }

      for (let subType = unionKind.subTypes; subType != null;
           subType = subType->next) {
        let comments = printComments(
            subType->decl->comments,
            indent + indent_width,
            subType->decl->location->line);
        printIndent(indent + indent_width);

        let structType = subType->decl->type->kind as TypeKind::Struct*;
        printToken(structType->tag);
        trailing = printStructBody(
            subType->decl,
            indent + indent_width,
            trailing);

        if (subType->next == null) {
          fprintf(printFile, "\n");
        } else {
          let lineDiff =
              subType->next->decl->location->line - subType->decl->endLocation->line;
          if (lineDiff > 1) {
            fprintf(printFile, "\n\n");
          } else {
            fprintf(printFile, "\n");
          }
        }
      }

      trailing = printComments(
          trailing,
          indent + indent_width,
          decl->endLocation->line);
      fprintf(printFile, "}");
    case DeclKind::Var:
      printLet(decl, indent);
      fprintf(printFile, ";");
    case DeclKind::Const:
      printLet(decl, indent);
      fprintf(printFile, ";");
    case DeclKind::Func as funcKind:
      if (funcKind.isExtern) {
        fprintf(printFile, "extern ");
      }
      fprintf(printFile, "func ");
      printToken(decl->name);

      // Print type parameters if present
      let fnType = decl->type->kind as TypeKind::Func*;
      if (fnType->typeArgs != null) {
        fprintf(printFile, "[");
        for (let tp = fnType->typeArgs; tp != null; tp = tp->next) {
          let tag = &tp->kind as TypeKind::Tag*;
          printToken(tag->tag);
          if (tp->next != null) {
            fprintf(printFile, ", ");
          }
        }
        fprintf(printFile, "]");
      }

      fprintf(printFile, "(");
      let isVarargs = fnType->isVarargs;
      let split = false;

      for (let arg: DeclAST* = funcKind.args; arg != null;
           arg = arg->next) {
        if (arg->next != null
            && arg->location->line != arg->next->location->line) {
          split = true;
        }
      }
      for (let arg: DeclAST* = funcKind.args; arg != null;
           arg = arg->next) {
        if (split) {
          fprintf(printFile, "\n");
          printIndent(indent + 2 * indent_width);
        }
        printToken(arg->name);
        fprintf(printFile, ": ");
        printType(arg->type);
        if (arg->next != null || isVarargs) {
          fprintf(printFile, ",");
          if (!split) {
            fprintf(printFile, " ");
          }
        }
      }
      if (isVarargs) {
        if (split) {
          fprintf(printFile, "\n");
          printIndent(indent + 2 * indent_width);
        }
        fprintf(printFile, "...");
      }
      if (split) {
        fprintf(printFile, "\n");
      }
      fprintf(printFile, ")");
      if (fnType->result->kind as TypeKind::Void* == null) {
        fprintf(printFile, " -> ");
        printType(fnType->result);
      }

      if (funcKind.body != null) {
        fprintf(printFile, " ");
        printStmt(funcKind.body);
      } else {
        fprintf(printFile, ";");
      }

    case DeclKind::Import as importKind:
      fprintf(printFile, "import ");
      printExpr(importKind.path);
      fprintf(printFile, ";");

    case DeclKind::EnumField:
      unreachable("Enum field outside enum");
  }

  if (trailing != null) {
    fprintf(printFile, " ");
    printComments(trailing, indent, 0);
  }
}

func printDecl(decl: DeclAST*) {
  printDeclIndent(decl, 0);
}

func allowNoNewline(decl: DeclAST*, declNext: DeclAST*) -> bool {
  // Hack that relies on internal representation of union kind tags.
  let kind1: i32 = 0;
  memcpy(&kind1, &decl->kind, sizeof(typeof(kind1)));
  let kind2: i32 = 0;
  memcpy(&kind2, &declNext->kind, sizeof(typeof(kind2)));

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
      let lineDiff = decl->next->location->line - decl->endLocation->line;
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
      fprintf(printFile, "\n");
    }
  }
}
