import ast;

struct DeclList {
  decl : DeclAST *;

  next : DeclList *;
};

struct ImportList {
  name : Token;
  next : ImportList *;
};

struct SemaState {
  parent : SemaState *;

  // Return type of the current function.
  result : Type *;

  switchType : Type *;

  // Local variables and enum fields.
  locals : DeclList *;

  // struct tag list.
  types : DeclList *;

  // Extra decls added during sema, used for string literals.
  // Should only be added to the root sema state.
  extraDecls : DeclAST *;
  strCount : i32;

  imports : ImportList *;
};

// 2. sema
func failSema(msg : const i8 *) {
  puts(msg);
  exit(1);
}

func failSemaExpr(msg : const i8 *, expr : ExprAST *) {
  printExpr(expr);
  printf("\n");
  failSema(msg);
}

func getRoot(state : SemaState *) -> SemaState * {
  while (state->parent != NULL) {
    state = state->parent;
  }
  return state;
}

func typeEq(one : Type *, two : Type *) -> i32 {
  if (one->kind != two->kind) {
    return 0;
  }

  switch (one->kind) {
  case TypeKind::VOID:
    break;

  case TypeKind::INT:
    return one->isSigned == two->isSigned && one->size == two->size;

  case TypeKind::ARRAY:
    if (one->size != 0 && two->size != 0 && one->size != two->size) {
      return 0;
    }
  case TypeKind::POINTER:
    return typeEq(one->arg, two->arg);

  case TypeKind::STRUCT:
  case TypeKind::ENUM:
    return tokCmp(one->tag, two->tag);

  case TypeKind::FUNC:
    failSema("TODO: type eq func");
    break;

  default:
    failSema("Unknown type for typeEq");
  }

  return 1;
}

func doConvert(expr : ExprAST *, to : Type *) -> ExprAST * {
  let from = expr->type;

  if (typeEq(from, to)) {
    return expr;
  }

  // Allow integer expression casting
  if (to->kind == TypeKind::INT && expr->kind == ExprKind::INT) {
    let res = newExpr(ExprKind::INT);
    res->value = expr->value;
    res->type = to;
    return res;
  }

  // TODO: Remove and use 'as' once the ': type' syntax is implemented.
  // void * can be converted from and to any other pointer..
  if (from->kind == TypeKind::POINTER && to->kind == TypeKind::POINTER &&
      (from->arg->kind == TypeKind::VOID || to->arg->kind == TypeKind::VOID)) {
    return expr;
  }

  return NULL;
}

func canCast(expr : ExprAST *, to : Type *) -> i32 {
  let from = expr->type;

  if (typeEq(from, to)) {
    return 1;
  }

  // Sign change, no-op for now.
  if (from->kind == TypeKind::INT && to->kind == TypeKind::INT &&
      from->size == to->size) {
    return 1;
  }

  // Same signedness but different type
  if (from->kind == TypeKind::INT && to->kind == TypeKind::INT &&
      from->size != to->size) {
    return 1;
  }

  // void * can be converted from and to any other pointer..
  if (from->kind == TypeKind::POINTER && to->kind == TypeKind::POINTER &&
      (from->arg->kind == TypeKind::VOID || to->arg->kind == TypeKind::VOID)) {
    return 1;
  }

  // enums can be casted to integers
  if (from->kind == TypeKind::ENUM && to->kind == TypeKind::INT) {
    return 1;
  }

  // and vice versa
  if (from->kind == TypeKind::INT && to->kind == TypeKind::ENUM) {
    return 1;
  }

  return 0;
}

func checkBool(expr : ExprAST *) {
  if (expr->type->kind != TypeKind::INT) {
    printExpr(expr);
    failSema(": Expected bool!");
  }
}

func newDeclList(decl : DeclAST *) -> DeclList * {
  let res : DeclList * = calloc(1, sizeof(struct DeclList));
  res->decl = decl;
  return res;
}

func findType(types : DeclList *, tag : Token) -> DeclAST * {
  for (; types != NULL; types = types->next) {
    if (tokCmp(tag, types->decl->type->tag)) {
      return types->decl;
    }
  }
  return NULL;
}

func lookupType(state : SemaState *, tag : Token) -> DeclAST * {
  for (; state != NULL; state = state->parent) {
    let type = findType(state->types, tag);
    if (type != NULL) {
      return type;
    }
  }

  printToken(tag);
  failSema(": Unknown type for scope");
  return NULL;
}

func findLocal(local : DeclList *, name : Token) -> DeclAST * {
  for (; local != NULL; local = local->next) {
    if (tokCmp(name, local->decl->name)) {
      return local->decl;
    }
  }
  return NULL;
}

func lookupLocal(state : SemaState *, name : Token) -> DeclAST * {
  for (; state != NULL; state = state->parent) {
    let local = findLocal(state->locals, name);
    if (local != NULL) {
      return local;
    }
  }

  printToken(name);
  failSema("Unknown local");
  return NULL;
}

func findField(structDecl : DeclAST *, name : Token, idxOut : i32 *)
    -> DeclAST * {
  let idx = 0;
  for (let field = structDecl->fields; field != NULL;
       field = field->next, idx++) {
    if (tokCmp(name, field->name)) {
      *idxOut = idx;
      return field;
    }
  }
  return NULL;
}

func getSize(state : SemaState *, type : Type *) -> i32 {
  switch (type->kind) {
  case TypeKind::VOID:
    return 0;

    // default enum is i32 = 4 bytes.
  case TypeKind::ENUM:
    return 4;

  case TypeKind::INT:
    return type->size / 8;

  case TypeKind::POINTER:
  case TypeKind::FUNC:
    return 8;

    // TODO: padding
  case TypeKind::ARRAY:
    return type->size * getSize(state, type->arg);
  case TypeKind::STRUCT: {
    let decl = lookupType(state, type->tag);
    let size = 0;
    for (let field = decl->fields; field != NULL; field = field->next) {
      size += getSize(state, field->type);
    }
    return size == 0 ? 1 : size;
  }
  default:
    failSema("Unknown type for size");
    return 0;
  }
}

func semaExprNoDecay(state : SemaState *, expr : ExprAST *);

func semaExpr(state : SemaState *, expr : ExprAST *) {
  semaExprNoDecay(state, expr);

  if (expr->kind == ExprKind::STR) {
    // Add a global variable for the string.
    let root = getRoot(state);
    let decl = newDecl();
    decl->kind = DeclKind::VAR;
    decl->type = expr->type;

    let name : i8 * = malloc(32 as u64);
    let n = sprintf(name, "str.%d", root->strCount++);
    decl->name.kind = TokenKind::IDENTIFIER;
    decl->name.data = name;
    decl->name.end = name + n;

    decl->init = newExpr(ExprKind::STR);
    decl->init->identifier = expr->identifier;
    decl->init->type = expr->type;

    decl->next = root->extraDecls;
    root->extraDecls = decl;

    // Transmute expr into a variable ref.
    expr->kind = ExprKind::VARIABLE;
    expr->identifier = decl->name;

    // This should trigger the decay below...
    expr->type = decl->type;
  }

  // Decay array to pointer
  // TODO: cast expr
  if (expr->type->kind == TypeKind::ARRAY) {
    let decayType = newType(TypeKind::POINTER);
    decayType->arg = expr->type->arg;
    decayType->isDecay = 1;
    expr->type = decayType;
  }
}

func semaBinExpr(state : SemaState *, expr : ExprAST *) {
  semaExpr(state, expr->lhs);
  semaExpr(state, expr->rhs);

  switch (expr->op.kind) {
  case TokenKind::COMMA:
    expr->type = expr->rhs->type;
    break;

    // comparision results in i32.
  case TokenKind::LESS:
  case TokenKind::GREATER:
  case TokenKind::LE_OP:
  case TokenKind::GE_OP:
  case TokenKind::EQ_OP:
  case TokenKind::NE_OP:
    if (!typeEq(expr->lhs->type, expr->rhs->type)) {
      let lhsConv = doConvert(expr->lhs, expr->rhs->type);

      if (lhsConv == NULL) {
        let rhsConv = doConvert(expr->rhs, expr->lhs->type);
        if (rhsConv == NULL) {
          printExpr(expr);
          failSema(": Binary op on different types");
        }
        expr->rhs = rhsConv;
      } else {
        expr->lhs = lhsConv;
      }
    }
    expr->type = getInt32();
    return;

  case TokenKind::MINUS:
    if (expr->lhs->type->kind == TypeKind::POINTER &&
        expr->rhs->type->kind == TypeKind::POINTER) {

      if (expr->lhs->type->arg->kind != TypeKind::INT ||
          expr->lhs->type->arg->size != 8 ||
          expr->rhs->type->arg->kind != TypeKind::INT ||
          expr->rhs->type->arg->size != 8) {
        // TODO: emit (expr) / sizeof(type)
        failSema("Only char pointer subtract supported");
      }

      expr->type = getIPtr();
      break;
    }
  case TokenKind::PLUS:
    if (expr->op.kind != TokenKind::MINUS &&
        expr->lhs->type->kind == TypeKind::INT &&
        expr->rhs->type->kind == TypeKind::POINTER) {
      expr->type = expr->rhs->type;
      break;
    }
  case TokenKind::ADD_ASSIGN:
  case TokenKind::SUB_ASSIGN:
    if (expr->lhs->type->kind == TypeKind::POINTER &&
        expr->rhs->type->kind == TypeKind::INT) {
      expr->type = expr->lhs->type;
      break;
    }

  default: {
    if (isAssign(expr->op)) {
      let conv = doConvert(expr->rhs, expr->lhs->type);
      if (conv == NULL) {
        printExpr(expr);
        failSema(": Assign doesn't match");
      }
      expr->rhs = conv;
      expr->type = expr->lhs->type;
      break;
    }

    if (!typeEq(expr->lhs->type, expr->rhs->type)) {
      let lhsConv = doConvert(expr->lhs, expr->rhs->type);
      if (lhsConv == NULL) {
        let rhsConv = doConvert(expr->rhs, expr->lhs->type);
        if (rhsConv == NULL) {
          printExpr(expr);
          failSema(": type mismatch");
        }
        expr->rhs = rhsConv;
      } else {
        expr->lhs = lhsConv;
      }
    }

    expr->type = expr->lhs->type;
  }
  }
}

func getStringLength(tok : Token) -> i32 {
  let len = 0;

  for (let c = tok.data; c < tok.end; c++) {
    if (*c == '\\') {
      c++;
      len++;
    } else {
      len++;
    }
  }

  return len + 1; // null terminator
}

func semaExprNoDecay(state : SemaState *, expr : ExprAST *) {
  switch (expr->kind) {
  case ExprKind::ARG_LIST:
    failSema("TODO: sema all exprs");
    break;

  case ExprKind::SCOPE: {
    let decl = lookupType(state, expr->parent);
    if (decl == NULL || decl->type->kind != TypeKind::ENUM) {
      failSema("Expected enum type for scope expr");
    }

    let fieldDecl = findField(decl, expr->identifier, &expr->value);
    if (fieldDecl == NULL) {
      printToken(expr->identifier);
      failSema(" Cannot find field");
    }

    expr->type = newType(TypeKind::ENUM);
    expr->type->tag = decl->type->tag;
  } break;

  case ExprKind::MEMBER:
    semaExpr(state, expr->lhs);
    let structDecl = NULL;
    if (expr->op.kind == TokenKind::PTR_OP) {
      if (expr->lhs->type->kind != TypeKind::POINTER ||
          expr->lhs->type->arg->kind != TypeKind::STRUCT) {
        printExpr(expr);
        failSema(": Expected pointer to struct type for -> expr");
      }
      structDecl = lookupType(state, expr->lhs->type->arg->tag);
    } else if (expr->op.kind == TokenKind::DOT) {
      if (expr->lhs->type->kind != TypeKind::STRUCT) {
        failSema("Expected struct type for . expr");
      }
      structDecl = lookupType(state, expr->lhs->type->tag);
    } else {
      failSema("Unknown member op");
    }
    let fieldDecl = findField(structDecl, expr->identifier, &expr->value);
    if (fieldDecl == NULL) {
      printToken(expr->identifier);
      failSema(" Cannot find field");
    }
    expr->type = fieldDecl->type;
    break;

  case ExprKind::CALL:
    semaExpr(state, expr->lhs);
    // We don't support function pointers
    if (expr->lhs->type->kind != TypeKind::FUNC) {
      failSema("Must call function type");
    }
    let curArgTy = expr->lhs->type->arg;
    let cur = expr->rhs;
    for (; cur != NULL; cur = cur->rhs) {
      semaExpr(state, cur->lhs);

      if (curArgTy != NULL) {
        let conv = doConvert(cur->lhs, curArgTy);
        if (conv == NULL) {
          printExpr(expr);
          printType(curArgTy);
          failSema(" Arg type mismatch");
        }
        cur->lhs = conv;
      }
      if (curArgTy != NULL) {
        curArgTy = curArgTy->argNext;
      }
    }
    let isValidVararg = expr->lhs->type->isVarargs && curArgTy == NULL;
    if (!isValidVararg && (curArgTy == NULL) != (cur == NULL)) {
      failSema("Function call arg length mismatch");
    }
    expr->type = expr->lhs->type->result;
    break;

  case ExprKind::CONDITIONAL:
    semaExpr(state, expr->cond);
    checkBool(expr->cond);
    semaExpr(state, expr->lhs);
    semaExpr(state, expr->rhs);
    if (!typeEq(expr->lhs->type, expr->rhs->type)) {
      failSema("?: lhs and rhs should have same type");
    }
    expr->type = expr->lhs->type;
    break;

  case ExprKind::ARRAY:
    expr->type = newType(TypeKind::ARRAY);
    for (let sub = expr; sub != NULL; sub = sub->rhs) {
      semaExpr(state, sub->lhs);

      if (expr->type->arg == NULL) {
        expr->type->arg = sub->lhs->type;
      } else if (!typeEq(expr->type->arg, sub->lhs->type)) {
        failSema("Init must have consistent type");
      }

      expr->type->size++;
    }
    break;

  case ExprKind::STR:
    expr->type = newType(TypeKind::ARRAY);
    expr->type->arg = newType(TypeKind::INT);
    expr->type->arg->size = 8;
    expr->type->arg->isSigned = 1;

    expr->type->size = getStringLength(expr->identifier);

    expr->type->isConst = 1;
    break;

  case ExprKind::VARIABLE: {
    let local = lookupLocal(state, expr->identifier);

    // enum value, transform this expr to an i32.
    if (local->kind == DeclKind::ENUM_FIELD) {
      expr->kind = ExprKind::INT;
      expr->value = local->enumValue;
    }

    expr->type = local->type;
  } break;

  case ExprKind::INT:
    expr->type = getInt32();
    return;

  case ExprKind::BINARY:
    semaBinExpr(state, expr);
    break;

  case ExprKind::INDEX:
    semaExpr(state, expr->lhs);
    if (expr->lhs->type->kind != TypeKind::POINTER &&
        expr->lhs->type->kind != TypeKind::ARRAY) {
      failSema("Can't index non array or pointer");
    }
    semaExpr(state, expr->rhs);
    if (expr->rhs->type->kind != TypeKind::INT) {
      failSemaExpr("Can't index with non integer", expr);
    }
    expr->type = expr->lhs->type->arg;
    break;

  case ExprKind::UNARY:
    if (expr->op.kind == TokenKind::AND) {
      semaExprNoDecay(state, expr->rhs);
      expr->type = newType(TypeKind::POINTER);
      expr->type->arg = expr->rhs->type;
      return;
    }

    if (expr->lhs != NULL) {
      semaExpr(state, expr->lhs);
      expr->type = expr->lhs->type;
    } else {
      semaExpr(state, expr->rhs);
      expr->type = expr->rhs->type;
    }

    // Handle the specials
    if (expr->op.kind == TokenKind::STAR) {
      if (expr->rhs->type->kind != TypeKind::POINTER) {
        failSema("Expected pointer type for *");
      }
      expr->type = expr->type->arg;
    }

    // TODO: correct?
    if (expr->op.kind == TokenKind::BANG) {
      expr->type = getInt32();
    }

    break;

  case ExprKind::SIZEOF:
    if (expr->rhs != NULL) {
      semaExprNoDecay(state, expr->rhs);
      expr->value = getSize(state, expr->rhs->type);
    } else {
      expr->value = getSize(state, expr->sizeofArg);
    }
    expr->kind = ExprKind::INT;
    expr->type = getUPtr();
    break;

  case ExprKind::CAST:
    semaExpr(state, expr->lhs);
    if (expr->type == NULL) {
      failSema("Cast without type?");
    }
    if (!canCast(expr->lhs, expr->type)) {
      printType(expr->lhs->type);
      printf(" -> ");
      printType(expr->type);
      failSema(" Can't cast");
    }
    break;
  }
}

func semaStmt(state : SemaState *, stmt : StmtAST *);

func semaTopLevel(state : SemaState *, decl : DeclAST *) -> DeclAST *;

func addLocalDecl(state : SemaState *, decl : DeclAST *) {
  let prev = findLocal(state->locals, decl->name);
  if (prev != NULL) {
    // Allow redef of functions, TODO: verify type match...
    if (prev->kind == DeclKind::FUNC && prev->body == NULL) {
      prev->hasDef = 1;
    } else {
      printToken(decl->name);
      failSema("Variable redef");
    }
  }

  let newLocal = newDeclList(decl);
  newLocal->next = state->locals;
  state->locals = newLocal;
}

func resolveTypeTags(state : SemaState *, type : Type *) {
  if (type == NULL) {
    return;
  }

  if (type->kind == TypeKind::TAG) {
    let typeDecl = lookupType(state, type->tag);
    type->kind = typeDecl->type->kind;
  }

  resolveTypeTags(state, type->result);
  resolveTypeTags(state, type->arg);
  resolveTypeTags(state, type->argNext);
}

func semaDecl(state : SemaState *, decl : DeclAST *) {
  resolveTypeTags(state, decl->type);

  switch (decl->kind) {
  case DeclKind::STRUCT:
    // Resolve tags in fields.
    for (let field = decl->fields; field != NULL; field = field->next) {
      if (field->kind != DeclKind::VAR) {
        failSema("Only var decls allowed in struct");
      }

      resolveTypeTags(state, field->type);
    }
    break;

  case DeclKind::ENUM:
    break;

  case DeclKind::FUNC:
    addLocalDecl(state, decl);
    if (decl->body != NULL) {
      let funcState : SemaState = {0};
      funcState.parent = state;
      funcState.result = decl->type->result;

      // Generate a local for each arg.
      for (let field = decl->fields; field != NULL; field = field->next) {
        resolveTypeTags(state, field->type);
        addLocalDecl(&funcState, field);
      }
      semaStmt(&funcState, decl->body);
    }
    break;

  case DeclKind::VAR:
    addLocalDecl(state, decl);
    if (decl->init != NULL) {
      if (decl->type != NULL && decl->type->kind == TypeKind::ARRAY) {
        semaExprNoDecay(state, decl->init);
      } else {
        semaExpr(state, decl->init);
      }

      if (decl->type == NULL) {
        decl->type = decl->init->type;
      } else if (decl->type->kind == TypeKind::STRUCT &&
                 decl->init->kind == ExprKind::ARRAY) {
        // TODO: verify match?
        if (decl->init->rhs != NULL || decl->init->lhs->kind != ExprKind::INT) {
          failSema("Currently only zero init supported");
        }
        decl->init->type = decl->type;
      } else if (decl->init = doConvert(decl->init, decl->type),
                 decl->init == NULL) {
        printDecl(decl);
        failSema(": Decl init type doesn't match");
      }

      if (decl->type->kind == TypeKind::ARRAY && decl->type->size == 0) {
        decl->type->size = decl->init->type->size;
      }
    }
    break;
  case DeclKind::IMPORT: {
    if (state->parent != NULL) {
      failSema("Import not allowed in local scope");
    }

    // Check if we already import this one
    for (let cur = state->imports; cur != NULL; cur = cur->next) {
      if (tokCmp(decl->name, cur->name)) {
        return;
      }
    }

    // Add to imports
    let imports : ImportList * = calloc(1, sizeof(struct ImportList));
    imports->name = decl->name;
    imports->next = state->imports;
    state->imports = imports;

    let buf : i8 * = malloc(256);
    sprintf(buf, "%.*s.b", decl->name.end - decl->name.data, decl->name.data);
    let fileDecls = parseFile(buf);
    if (fileDecls == NULL) {
      failSema("Failed to import file");
    }

    // semaTopLevel will return a combined list of decls from the file and the
    // extraDecls.
    let extras = semaTopLevel(state, fileDecls);
    state->extraDecls = extras;

  } break;

  case DeclKind::ENUM_FIELD:
    failSema("Shoudln't happen");
    return;

  default:
    failSema("Unknown decl kind");
  }
}

func newState(parent : SemaState *) -> SemaState {
  let state : SemaState = {0};
  state.parent = parent;
  state.result = parent->result;
  state.switchType = parent->switchType;
  return state;
}

func semaStmt(state : SemaState *, stmt : StmtAST *) {
  switch (stmt->kind) {
  case StmtKind::EXPR:
    if (stmt->expr != NULL) {
      semaExpr(state, stmt->expr);
    }
    break;
  case StmtKind::DECL:
    if (stmt->decl->kind != DeclKind::VAR) {
      failSema("Only var decls allowed in local scope");
    }
    return semaDecl(state, stmt->decl);

  case StmtKind::RETURN:
    if (stmt->expr == NULL && state->result->kind != TypeKind::VOID) {
      failSema("Return type should be void");
    }
    if (stmt->expr != NULL) {
      semaExpr(state, stmt->expr);
      let conv = doConvert(stmt->expr, state->result);
      if (conv == NULL) {
        printStmt(stmt);
        return failSema("Return type mismatch");
      }
      stmt->expr = conv;
    }
    break;

  case StmtKind::COMPOUND: {
    let subState = newState(state);

    for (let cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      semaStmt(&subState, cur);
    }
  } break;

  case StmtKind::IF:
    semaExpr(state, stmt->expr);
    checkBool(stmt->expr);

    semaStmt(state, stmt->init);
    if (stmt->stmt != NULL) {
      semaStmt(state, stmt->stmt);
    }
    break;

  case StmtKind::WHILE:
    semaExpr(state, stmt->expr);
    checkBool(stmt->expr);
    semaStmt(state, stmt->stmt);
    break;

  case StmtKind::FOR: {
    let subState = newState(state);
    semaStmt(&subState, stmt->init);
    // cond must be expr stmt.
    semaExpr(&subState, stmt->cond->expr);
    checkBool(stmt->cond->expr);
    semaExpr(&subState, stmt->expr);

    semaStmt(&subState, stmt->stmt);
  } break;

  case StmtKind::SWITCH: {
    let subState = newState(state);

    semaExpr(&subState, stmt->expr);
    subState.switchType = stmt->expr->type;

    if (stmt->expr->type->kind != TypeKind::INT &&
        stmt->expr->type->kind != TypeKind::ENUM) {
      printType(stmt->expr->type);
      failSema("Switch expr must be integer or enum");
    }

    semaStmt(&subState, stmt->stmt);
  } break;

  case StmtKind::CASE:
    semaExpr(state, stmt->expr);
    if (!typeEq(stmt->expr->type, state->switchType)) {
      printStmt(stmt);
      printType(state->switchType);
      failSema("case expr must match switch type");
    }
    break;

  case StmtKind::BREAK:
  case StmtKind::DEFAULT:
    break;
  }
}

func addTaggedType(state : SemaState *, decl : DeclAST *) {
  switch (decl->kind) {
  case DeclKind::STRUCT:
  case DeclKind::ENUM:
    if (findType(state->types, decl->type->tag) != NULL) {
      printDecl(decl);
      failSema(": Type redef");
    }

    // Add the struct to the types.
    let type = newDeclList(decl);
    type->next = state->types;
    state->types = type;
    break;
  }
}

func semaTopLevel(state : SemaState *, decl : DeclAST *) -> DeclAST * {
  // Discover tagged types.
  for (let cur = decl; cur != NULL; cur = cur->next) {
    addTaggedType(state, cur);
  }

  for (let cur = decl; cur != NULL; cur = cur->next) {
    semaDecl(state, cur);
  }

  // Add extra decls
  if (state->extraDecls != NULL) {
    let last = state->extraDecls;
    while (last->next != NULL) {
      last = last->next;
    }
    last->next = decl;
    decl = state->extraDecls;
  }

  return decl;
}

func initSemaState() -> SemaState {
  let semaState : SemaState = {0};
  let nullTok : Token;
  nullTok.kind = TokenKind::IDENTIFIER;
  nullTok.data = "NULL";
  nullTok.end = nullTok.data + 4;

  // Add null as a nullptr
  let nullDecl = newDecl();
  nullDecl->kind = DeclKind::ENUM_FIELD;
  nullDecl->name = nullTok;
  nullDecl->enumValue = 0;
  nullDecl->type = newType(TypeKind::POINTER);
  nullDecl->type->arg = newType(TypeKind::VOID);

  semaState.locals = newDeclList(nullDecl);
  return semaState;
}
