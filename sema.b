import ast;

struct DeclList {
  struct DeclAST *decl;

  struct DeclList *next;
};

struct ImportList {
  struct Token name;
  struct ImportList *next;
};

struct SemaState {
  struct SemaState *parent;

  // Return type of the current function.
  struct Type *result;

  struct Type *switchType;

  // Local variables and enum fields.
  struct DeclList *locals;

  // struct tag list.
  struct DeclList *types;

  // Extra decls added during sema, used for string literals.
  // Should only be added to the root sema state.
  struct DeclAST *extraDecls;
  i32 strCount;

  struct ImportList *imports;
};

// 2. sema
void failSema(const i8 *msg) {
  puts(msg);
  exit(1);
}

struct SemaState *getRoot(struct SemaState *state) {
  while (state->parent != NULL) {
    state = state->parent;
  }
  return state;
}

i32 typeEq(struct Type *one, struct Type *two) {
  if (one->kind != two->kind) {
    return 0;
  }

  switch (one->kind) {
  case VOID_TYPE:
    break;

  case INT_TYPE2:
    return one->isSigned == two->isSigned && one->size == two->size;

  case ARRAY_TYPE:
    if (one->size != 0 && two->size != 0 && one->size != two->size) {
      return 0;
    }
  case POINTER_TYPE:
    return typeEq(one->arg, two->arg);

  case STRUCT_TYPE:
    return tokCmp(one->tag, two->tag);

  case FUNC_TYPE:
    failSema("TODO: type eq func");
    break;
  }

  return 1;
}

struct ExprAST *doConvert(struct ExprAST *expr, struct Type *to) {
  struct Type *from = expr->type;

  if (typeEq(from, to)) {
    return expr;
  }

  // Allow integer expression casting
  if (to->kind == INT_TYPE2 && expr->kind == INT_EXPR) {
    struct ExprAST *res = newExpr(INT_EXPR);
    res->value = expr->value;
    res->type = to;
    return res;
  }

  // TODO: Remove and use 'as' once the ': type' syntax is implemented.
  // void * can be converted from and to any other pointer..
  if (from->kind == POINTER_TYPE && to->kind == POINTER_TYPE &&
      (from->arg->kind == VOID_TYPE || to->arg->kind == VOID_TYPE)) {
    return expr;
  }

  return NULL;
}

i32 canCast(struct ExprAST *expr, struct Type *to) {
  struct Type *from = expr->type;

  if (typeEq(from, to)) {
    return 1;
  }

  // Sign change, no-op for now.
  if (from->kind == INT_TYPE2 && to->kind == INT_TYPE2 &&
      from->size == to->size) {
    return 1;
  }

  // Same signedness but different type
  if (from->kind == INT_TYPE2 && to->kind == INT_TYPE2 &&
      from->size != to->size) {
    return 1;
  }

  // void * can be converted from and to any other pointer..
  if (from->kind == POINTER_TYPE && to->kind == POINTER_TYPE &&
      (from->arg->kind == VOID_TYPE || to->arg->kind == VOID_TYPE)) {
    return 1;
  }

  return 0;
}

void checkBool(struct ExprAST *expr) {
  if (expr->type->kind != INT_TYPE2) {
    printExpr(expr);
    failSema(": Expected bool!");
  }
}

struct DeclList *newDeclList(struct DeclAST *decl) {
  struct DeclList *res = calloc(1, sizeof(struct DeclList));
  res->decl = decl;
  return res;
}

struct DeclAST *findType(struct DeclList *types, struct Token tag) {
  for (; types != NULL; types = types->next) {
    if (tokCmp(tag, types->decl->type->tag)) {
      return types->decl;
    }
  }
  return NULL;
}

struct DeclAST *lookupType(struct SemaState *state, struct Token tag) {
  for (; state != NULL; state = state->parent) {
    struct DeclAST *type = findType(state->types, tag);
    if (type != NULL) {
      return type;
    }
  }

  printToken(tag);
  failSema("Unknown type");
  return NULL;
}

struct DeclAST *findLocal(struct DeclList *local, struct Token name) {
  for (; local != NULL; local = local->next) {
    if (tokCmp(name, local->decl->name)) {
      return local->decl;
    }
  }
  return NULL;
}

struct DeclAST *lookupLocal(struct SemaState *state, struct Token name) {
  for (; state != NULL; state = state->parent) {
    struct DeclAST *local = findLocal(state->locals, name);
    if (local != NULL) {
      return local;
    }
  }

  printToken(name);
  failSema("Unknown local");
  return NULL;
}

struct DeclAST *findField(struct DeclAST *structDecl, struct Token name,
                          i32 *idxOut) {
  i32 idx = 0;
  for (struct DeclAST *field = structDecl->fields; field != NULL;
       field = field->next, idx++) {
    if (tokCmp(name, field->name)) {
      *idxOut = idx;
      return field;
    }
  }
  return NULL;
}

i32 getSize(struct SemaState *state, struct Type *type) {
  switch (type->kind) {
  case VOID_TYPE:
    return 0;
  case INT_TYPE2:
    return type->size / 8;

  case POINTER_TYPE:
  case FUNC_TYPE:
    return 8;

    // TODO: padding
  case ARRAY_TYPE:
    return type->size * getSize(state, type->arg);
  case STRUCT_TYPE: {
    struct DeclAST *decl = lookupType(state, type->tag);
    i32 size = 0;
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
      size += getSize(state, field->type);
    }
    return size == 0 ? 1 : size;
  }
  default:
    failSema("Unknown type for size");
    return 0;
  }
}

void semaExprNoDecay(struct SemaState *state, struct ExprAST *expr);

void semaExpr(struct SemaState *state, struct ExprAST *expr) {
  semaExprNoDecay(state, expr);

  if (expr->kind == STR_EXPR) {
    // Add a global variable for the string.
    struct SemaState *root = getRoot(state);
    struct DeclAST *decl = newDecl();
    decl->kind = VAR_DECL;
    decl->type = expr->type;

    i8 *name = malloc(32 as u64);
    i32 n = sprintf(name, "str.%d", root->strCount++);
    decl->name.kind = IDENTIFIER;
    decl->name.data = name;
    decl->name.end = name + n;

    decl->init = newExpr(STR_EXPR);
    decl->init->identifier = expr->identifier;
    decl->init->type = expr->type;

    decl->next = root->extraDecls;
    root->extraDecls = decl;

    // Transmute expr into a variable ref.
    expr->kind = VARIABLE_EXPR;
    expr->identifier = decl->name;

    // This should trigger the decay below...
    expr->type = decl->type;
  }

  // Decay array to pointer
  // TODO: cast expr
  if (expr->type->kind == ARRAY_TYPE) {
    struct Type *decayType = newType(POINTER_TYPE);
    decayType->arg = expr->type->arg;
    decayType->isDecay = 1;
    expr->type = decayType;
  }
}

void semaBinExpr(struct SemaState *state, struct ExprAST *expr) {
  semaExpr(state, expr->lhs);
  semaExpr(state, expr->rhs);

  switch (expr->op.kind) {
  case COMMA:
    expr->type = expr->rhs->type;
    break;

    // comparision results in i32.
  case LESS:
  case GREATER:
  case LE_OP:
  case GE_OP:
  case EQ_OP:
  case NE_OP:
    if (!typeEq(expr->lhs->type, expr->rhs->type)) {
      struct ExprAST *lhsConv = doConvert(expr->lhs, expr->rhs->type);

      if (lhsConv == NULL) {
        struct ExprAST *rhsConv = doConvert(expr->rhs, expr->lhs->type);
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

  case MINUS:
    if (expr->lhs->type->kind == POINTER_TYPE &&
        expr->rhs->type->kind == POINTER_TYPE) {

      if (expr->lhs->type->arg->kind != INT_TYPE2 ||
          expr->lhs->type->arg->size != 8 ||
          expr->rhs->type->arg->kind != INT_TYPE2 ||
          expr->rhs->type->arg->size != 8) {
        // TODO: emit (expr) / sizeof(type)
        failSema("Only char pointer subtract supported");
      }

      expr->type = getIPtr();
      break;
    }
  case PLUS:
    if (expr->op.kind != MINUS && expr->lhs->type->kind == INT_TYPE2 &&
        expr->rhs->type->kind == POINTER_TYPE) {
      expr->type = expr->rhs->type;
      break;
    }
  case ADD_ASSIGN:
  case SUB_ASSIGN:
    if (expr->lhs->type->kind == POINTER_TYPE &&
        expr->rhs->type->kind == INT_TYPE2) {
      expr->type = expr->lhs->type;
      break;
    }

  default: {
    if (isAssign(expr->op)) {
      struct ExprAST *conv = doConvert(expr->rhs, expr->lhs->type);
      if (conv == NULL) {
        printExpr(expr);
        failSema(": Assign doesn't match");
      }
      expr->rhs = conv;
      expr->type = expr->lhs->type;
      break;
    }

    if (!typeEq(expr->lhs->type, expr->rhs->type)) {
      struct ExprAST *lhsConv = doConvert(expr->lhs, expr->rhs->type);
      if (lhsConv == NULL) {
        struct ExprAST *rhsConv = doConvert(expr->rhs, expr->lhs->type);
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

i32 getStringLength(struct Token tok) {
  i32 len = 0;

  for (i8 *c = tok.data; c < tok.end; c++) {
    if (*c == '\\') {
      c++;
      len++;
    } else {
      len++;
    }
  }

  return len + 1; // null terminator
}

void semaExprNoDecay(struct SemaState *state, struct ExprAST *expr) {
  switch (expr->kind) {
  case ARG_LIST:
    failSema("TODO: sema all exprs");
    break;

  case MEMBER_EXPR:
    semaExpr(state, expr->lhs);
    struct DeclAST *structDecl = NULL;
    if (expr->op.kind == PTR_OP) {
      if (expr->lhs->type->kind != POINTER_TYPE ||
          expr->lhs->type->arg->kind != STRUCT_TYPE) {
        failSema("Expected pointer to struct type for -> expr");
      }
      structDecl = lookupType(state, expr->lhs->type->arg->tag);
    } else if (expr->op.kind == DOT) {
      if (expr->lhs->type->kind != STRUCT_TYPE) {
        failSema("Expected struct type for . expr");
      }
      structDecl = lookupType(state, expr->lhs->type->tag);
    } else {
      failSema("Unknown member op");
    }
    struct DeclAST *fieldDecl =
        findField(structDecl, expr->identifier, &expr->value);
    if (fieldDecl == NULL) {
      printToken(expr->identifier);
      failSema(" Cannot find field");
    }
    expr->type = fieldDecl->type;
    break;

  case CALL_EXPR:
    semaExpr(state, expr->lhs);
    // We don't support function pointers
    if (expr->lhs->type->kind != FUNC_TYPE) {
      failSema("Must call function type");
    }
    struct Type *curArgTy = expr->lhs->type->arg;
    struct ExprAST *cur = expr->rhs;
    for (; cur != NULL; cur = cur->rhs) {
      semaExpr(state, cur->lhs);

      if (curArgTy != NULL) {
        struct ExprAST *conv = doConvert(cur->lhs, curArgTy);
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
    i32 isValidVararg = expr->lhs->type->isVarargs && curArgTy == NULL;
    if (!isValidVararg && (curArgTy == NULL) != (cur == NULL)) {
      failSema("Function call arg length mismatch");
    }
    expr->type = expr->lhs->type->result;
    break;

  case CONDITIONAL_EXPR:
    semaExpr(state, expr->cond);
    checkBool(expr->cond);
    semaExpr(state, expr->lhs);
    semaExpr(state, expr->rhs);
    if (!typeEq(expr->lhs->type, expr->rhs->type)) {
      failSema("?: lhs and rhs should have same type");
    }
    expr->type = expr->lhs->type;
    break;

  case ARRAY_EXPR:
    expr->type = newType(ARRAY_TYPE);
    for (struct ExprAST *sub = expr; sub != NULL; sub = sub->rhs) {
      semaExpr(state, sub->lhs);

      if (expr->type->arg == NULL) {
        expr->type->arg = sub->lhs->type;
      } else if (!typeEq(expr->type->arg, sub->lhs->type)) {
        failSema("Init must have consistent type");
      }

      expr->type->size++;
    }
    break;

  case STR_EXPR:
    expr->type = newType(ARRAY_TYPE);
    expr->type->arg = newType(INT_TYPE2);
    expr->type->arg->size = 8;
    expr->type->arg->isSigned = 1;

    expr->type->size = getStringLength(expr->identifier);

    expr->type->isConst = 1;
    break;

  case VARIABLE_EXPR: {
    struct DeclAST *local = lookupLocal(state, expr->identifier);

    // enum value, transform this expr to an i32.
    if (local->kind == ENUM_FIELD_DECL) {
      expr->kind = INT_EXPR;
      expr->value = local->enumValue;
    }

    expr->type = local->type;
  } break;

  case INT_EXPR:
    expr->type = getInt32();
    return;

  case BINARY_EXPR:
    semaBinExpr(state, expr);
    break;

  case INDEX_EXPR:
    semaExpr(state, expr->lhs);
    if (expr->lhs->type->kind != POINTER_TYPE &&
        expr->lhs->type->kind != ARRAY_TYPE) {
      failSema("Can't index non array or pointer");
    }
    semaExpr(state, expr->rhs);
    if (expr->rhs->type->kind != INT_TYPE2) {
      failSema("Can't index with non integer");
    }
    expr->type = expr->lhs->type->arg;
    break;

  case UNARY_EXPR:
    if (expr->op.kind == AND) {
      semaExprNoDecay(state, expr->rhs);
      expr->type = newType(POINTER_TYPE);
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
    if (expr->op.kind == STAR) {
      if (expr->rhs->type->kind != POINTER_TYPE) {
        failSema("Expected pointer type for *");
      }
      expr->type = expr->type->arg;
    }

    // TODO: correct?
    if (expr->op.kind == BANG) {
      expr->type = getInt32();
    }

    break;

  case SIZEOF_EXPR:
    if (expr->rhs != NULL) {
      semaExprNoDecay(state, expr->rhs);
      expr->value = getSize(state, expr->rhs->type);
    } else {
      expr->value = getSize(state, expr->sizeofArg);
    }
    expr->kind = INT_EXPR;
    expr->type = getUPtr();
    break;

  case CAST_EXPR:
    semaExpr(state, expr->lhs);
    if (expr->type == NULL) {
      failSema("Cast without type?");
    }
    if (!canCast(expr->lhs, expr->type)) {
      failSema("Can't cast");
    }
    break;
  }
}

struct SemaState initState() {
  struct SemaState semaState = {0};
  struct Token nullTok;
  nullTok.kind = IDENTIFIER;
  nullTok.data = "NULL";
  nullTok.end = nullTok.data + 4;

  // Add null as a nullptr
  struct DeclAST *nullDecl = newDecl();
  nullDecl->kind = ENUM_FIELD_DECL;
  nullDecl->name = nullTok;
  nullDecl->enumValue = 0;
  nullDecl->type = newType(POINTER_TYPE);
  nullDecl->type->arg = newType(VOID_TYPE);

  semaState.locals = newDeclList(nullDecl);
  return semaState;
}

void semaStmt(struct SemaState *state, struct StmtAST *stmt);

struct DeclAST *semaTopLevel(struct SemaState *state, struct DeclAST *decl);

void addLocalDecl(struct SemaState *state, struct DeclAST *decl) {
  struct DeclAST *prev = findLocal(state->locals, decl->name);
  if (prev != NULL) {
    // Allow redef of functions, TODO: verify type match...
    if (prev->kind == FUNC_DECL && prev->body == NULL) {
      prev->hasDef = 1;
    } else {
      printToken(decl->name);
      failSema("Variable redef");
    }
  }

  struct DeclList *newLocal = newDeclList(decl);
  newLocal->next = state->locals;
  state->locals = newLocal;
}

void semaDecl(struct SemaState *state, struct DeclAST *decl) {
  switch (decl->kind) {
  case ENUM_FIELD_DECL:
    addLocalDecl(state, decl);
    return;

  case STRUCT_DECL:
    if (findType(state->types, decl->type->tag) != NULL) {
      failSema("Type redef");
    }

    // Add sub-types to state, but ignore field var decls.
    struct SemaState subState = {0};
    subState.parent = state;
    subState.types = state->types;
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
      if (field->kind == ENUM_DECL || field->kind == STRUCT_DECL) {
        semaDecl(&subState, field);
      } else if (field->kind != VAR_DECL) {
        failSema("Field must be enum, struct or var.");
      }
    }
    state->types = subState.types;

    struct DeclList *type = newDeclList(decl);
    type->next = state->types;
    state->types = type;
    return;

  case ENUM_DECL: {
    addLocalDecl(state, decl);

    // TODO: this is wrong..
    struct SemaState *root = getRoot(state);

    // add a global/root var for reach field.
    for (struct DeclAST *field = decl->fields; field != NULL;
         field = field->next) {
      struct DeclList *newLocal = newDeclList(field);
      newLocal->next = root->locals;
      root->locals = newLocal;
    }
  } break;

  case FUNC_DECL:
    addLocalDecl(state, decl);
    if (decl->body != NULL) {
      struct SemaState funcState = {0};
      funcState.parent = state;
      funcState.result = decl->type->result;

      // Generate a local for each arg.
      for (struct DeclAST *field = decl->fields; field != NULL;
           field = field->next) {
        struct DeclList *newType = newDeclList(field);
        newType->next = funcState.locals;
        funcState.locals = newType;
      }
      semaStmt(&funcState, decl->body);
    }
    break;

  case VAR_DECL:
    addLocalDecl(state, decl);
    if (decl->init != NULL) {
      if (decl->type->kind == ARRAY_TYPE) {
        semaExprNoDecay(state, decl->init);
      } else {
        semaExpr(state, decl->init);
      }

      if (decl->type->kind == STRUCT_TYPE && decl->init->kind == ARRAY_EXPR) {
        // TODO: verify match?
        if (decl->init->rhs != NULL || decl->init->lhs->kind != INT_EXPR) {
          failSema("Currently only zero init supported");
        }
        decl->init->type = decl->type;
      } else if (decl->init = doConvert(decl->init, decl->type),
                 decl->init == NULL) {
        printDecl(decl);
        failSema(": Decl init type doesn't match");
      }
      if (decl->type->kind == ARRAY_TYPE && decl->type->size == 0) {
        decl->type->size = decl->init->type->size;
      }
    }
    break;
  case IMPORT_DECL: {
    if (state->parent != NULL) {
      failSema("Import not allowed in local scope");
    }

    // Check if we already import this one
    for (struct ImportList *cur = state->imports; cur != NULL;
         cur = cur->next) {
      if (tokCmp(decl->name, cur->name)) {
        return;
      }
    }

    // Add to imports
    struct ImportList *imports = calloc(1, sizeof(struct ImportList));
    imports->name = decl->name;
    imports->next = state->imports;
    state->imports = imports;

    i8 *buf = malloc(256);
    sprintf(buf, "%.*s.b", decl->name.end - decl->name.data, decl->name.data);
    struct DeclAST *fileDecls = parseFile(buf);
    if (fileDecls == NULL) {
      failSema("Failed to import file");
    }

    // semaTopLevel will return a combined list of decls from the file and the
    // extraDecls.
    struct DeclAST *extras = semaTopLevel(state, fileDecls);
    state->extraDecls = extras;

  } break;
  default:
    failSema("Unknown decl kind");
  }
}

struct SemaState newState(struct SemaState *parent) {
  struct SemaState state = {0};
  state.parent = parent;
  state.result = parent->result;
  state.switchType = parent->switchType;
  return state;
}

void semaStmt(struct SemaState *state, struct StmtAST *stmt) {
  switch (stmt->kind) {
  case EXPR_STMT:
    if (stmt->expr != NULL) {
      semaExpr(state, stmt->expr);
    }
    break;
  case DECL_STMT:
    if (stmt->decl->kind != VAR_DECL) {
      failSema("Only var decls allowed in local scope");
    }
    return semaDecl(state, stmt->decl);

  case RETURN_STMT:
    if (stmt->expr == NULL && state->result->kind != VOID_TYPE) {
      failSema("Return type should be void");
    }
    if (stmt->expr != NULL) {
      semaExpr(state, stmt->expr);
      struct ExprAST *conv = doConvert(stmt->expr, state->result);
      if (conv == NULL) {
        printStmt(stmt);
        return failSema("Return type mismatch");
      }
      stmt->expr = conv;
    }
    break;

  case COMPOUND_STMT: {
    struct SemaState subState = newState(state);

    for (struct StmtAST *cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
      semaStmt(&subState, cur);
    }
  } break;

  case IF_STMT:
    semaExpr(state, stmt->expr);
    checkBool(stmt->expr);

    semaStmt(state, stmt->init);
    if (stmt->stmt != NULL) {
      semaStmt(state, stmt->stmt);
    }
    break;

  case WHILE_STMT:
    semaExpr(state, stmt->expr);
    checkBool(stmt->expr);
    semaStmt(state, stmt->stmt);
    break;

  case FOR_STMT: {
    struct SemaState subState = newState(state);
    semaStmt(&subState, stmt->init);
    // cond must be expr stmt.
    semaExpr(&subState, stmt->cond->expr);
    checkBool(stmt->cond->expr);
    semaExpr(&subState, stmt->expr);

    semaStmt(&subState, stmt->stmt);
  } break;

  case SWITCH_STMT: {
    struct SemaState subState = newState(state);

    semaExpr(&subState, stmt->expr);
    subState.switchType = stmt->expr->type;

    if (stmt->expr->type->kind != INT_TYPE2) {
      printType(stmt->expr->type);
      failSema("Switch expr must be integer");
    }

    semaStmt(&subState, stmt->stmt);
  } break;

  case CASE_STMT:
    semaExpr(state, stmt->expr);
    if (!typeEq(stmt->expr->type, state->switchType)) {
      printStmt(stmt);
      printType(state->switchType);
      failSema("case expr must match switch type");
    }
    break;

  case BREAK_STMT:
  case DEFAULT_STMT:
    break;
  }
}

struct DeclAST *semaTopLevel(struct SemaState *state, struct DeclAST *decl) {
  for (struct DeclAST *cur = decl; cur != NULL; cur = cur->next) {
    semaDecl(state, cur);
  }

  // Add extra decls
  if (state->extraDecls != NULL) {
    struct DeclAST *last = state->extraDecls;
    while (last->next != NULL) {
      last = last->next;
    }
    last->next = decl;
    decl = state->extraDecls;
  }

  return decl;
}

