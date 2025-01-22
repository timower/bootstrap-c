import ast;
import parse;
import print_ast;


struct ImportList {
  name: Token;
  next: ImportList*;
};

struct SemaState {
  parent: SemaState*;

  // Return type of the current function.
  result: Type*;

  // Local variables and enum fields.
  locals: DeclList*;

  // struct tag list.
  types: DeclList*;

  // Extra decls added during sema, used for string literals.
  // Should only be added to the root sema state.
  extraDecls: DeclAST*;
  strCount: i32;

  imports: ImportList*;
};


// 2. sema
func failSema(loc: SourceLoc, msg: const i8*) {
  printf("%s:%d:%d: sema error: %s\n", loc.fileName, loc.line, loc.column, msg);
  exit(1);
}

func failSemaExpr(expr: ExprAST*, msg: const i8*) {
  printExpr(expr);
  printf("\n");
  failSema(expr->location, msg);
}

func failSemaDecl(decl: DeclAST*, msg: const i8*) {
  printDecl(decl);
  printf("\n");
  failSema(decl->location, msg);
}

func failSemaStmt(stmt: StmtAST*, msg: const i8*) {
  printStmt(stmt);
  printf("\n");
  failSema(stmt->location, msg);
}

func getRoot(state: SemaState*) -> SemaState* {
  while (state->parent != NULL) {
    state = state->parent;
  }
  return state;
}

func typeEq(one: Type*, two: Type*) -> i32 {
  if (one->kind != two->kind) {
    return 0;
  }

  switch (one->kind) {
    case TypeKind::VOID:
      break;
    case TypeKind::INT:
      return one->isSigned == two->isSigned && one->size == two->size;

    case TypeKind::ARRAY:
      if (one->size >= 0 && two->size >= 0 && one->size != two->size) {
        return 0;
      }
      return typeEq(one->arg, two->arg);
    case TypeKind::POINTER:
      return typeEq(one->arg, two->arg);

    case TypeKind::STRUCT, TypeKind::ENUM, TypeKind::UNION:
      return tokCmp(one->tag, two->tag);

    case TypeKind::FUNC:
      failSema(SourceLoc{}, "TODO: type eq func");
    case TypeKind::TAG, TypeKind::MEMBER_TAG:
      failSema(SourceLoc{}, "Type tag not resolved before eq");
  }

  return 1;
}

func findTypeIdx(types: DeclList*, tag: Token, idxOut: i32*) -> DeclAST* {
  let idx = 0;
  for (; types != NULL; types = types->next, idx++) {
    if (tokCmp(tag, types->decl->type->tag)) {
      if (idxOut != NULL) {
        *idxOut = idx;
      }
      return types->decl;
    }
  }
  return NULL;
}

func findType(types: DeclList*, tag: Token) -> DeclAST* {
  return findTypeIdx(types, tag, NULL);
}

func lookupType(state: SemaState*, tag: Token) -> DeclAST* {
  for (; state != NULL; state = state->parent) {
    let type = findType(state->types, tag);
    if (type != NULL) {
      return type;
    }
  }

  return NULL;
}

func doConvert(state: SemaState*, expr: ExprAST*, to: Type*) -> ExprAST* {
  let from = expr->type;

  if (typeEq(from, to)) {
    return expr;
  }

  // Allow integer expression casting
  if (to->kind == TypeKind::INT && expr->kind == ExprKind::INT) {
    let res = newExpr(ExprKind::INT);
    res->location = expr->location;
    res->value = expr->value;
    res->type = to;
    return res;
  }

  // TODO: Remove and use 'as' once the ': type' syntax is implemented.
  // void * can be converted from and to any other pointer..
  if (from->kind == TypeKind::POINTER && to->kind == TypeKind::POINTER
      && (from->arg->kind == TypeKind::VOID || to->arg->kind == TypeKind::VOID)) {
    return expr;
  }

  // Pointer to arrays can be convert to pointers to the first element.
  // This is a no-op for code gen?
  if (from->kind == TypeKind::POINTER && from->arg->kind == TypeKind::ARRAY
      && to->kind == TypeKind::POINTER
      && typeEq(from->arg->arg, to->arg)) {
    return expr;
  }

  // A union member can be converted to the union type by inserting the kind.
  if (from->kind == TypeKind::STRUCT && to->kind == TypeKind::UNION) {
    let unionDecl = lookupType(state, to->tag);
    if (unionDecl == NULL) {
      failSemaExpr(expr, "Can't find union decl to cast to");
    }

    let idx = 0;
    let structDecl = findTypeIdx(unionDecl->subTypes, from->tag, &idx);
    if (structDecl == NULL) {
      failSemaExpr(expr, "No way to convert struct to unrelated union");
    }

    let castExpr = newExpr(ExprKind::CAST);
    castExpr->location = expr->location;
    castExpr->lhs = expr;
    castExpr->type = to;
    castExpr->value = idx;
    return castExpr;
  }

  return NULL;
}

func resolveTypeTags(state: SemaState*, type: Type*) {
  if (type == NULL) {
    return;
  }

  if (type->kind == TypeKind::TAG) {
    let typeDecl = lookupType(state, type->tag);
    if (typeDecl == NULL) {
      failSema(SourceLoc{}, "Can't resolve type tags, unknown type");
    }
    type->kind = typeDecl->type->kind;
  } else if (type->kind == TypeKind::MEMBER_TAG) {
    let parentDecl = lookupType(state, type->parentTag);
    if (parentDecl == NULL) {
      failSema(SourceLoc{}, "Can't resolve type tags, unknown parent type");
    }

    let tagDecl = findType(parentDecl->subTypes, type->tag);
    if (tagDecl == NULL) {
      failSema(SourceLoc{}, "Can't resolve type tags, unknown sub type");
    }

    // TODO: share more?
    type->kind = tagDecl->type->kind;
    type->tag = tagDecl->type->tag;
  }

  resolveTypeTags(state, type->result);
  resolveTypeTags(state, type->arg);
  resolveTypeTags(state, type->argNext);
}

func semaCast(state: SemaState*, castExpr: ExprAST*) -> i32 {
  resolveTypeTags(state, castExpr->type);
  if (castExpr->type == NULL) {
    failSemaExpr(castExpr, "Cast without type?");
  }

  let to = castExpr->type;
  let expr = castExpr->lhs;
  let from = expr->type;

  if (typeEq(from, to)) {
    return 1;
  }

  // Sign change, no-op for now.
  if (from->kind == TypeKind::INT && to->kind == TypeKind::INT
      && from->size == to->size) {
    return 1;
  }

  // Same signedness but different type
  if (from->kind == TypeKind::INT && to->kind == TypeKind::INT
      && from->size != to->size) {
    return 1;
  }

  // void * can be converted from and to any other pointer..
  if (from->kind == TypeKind::POINTER && to->kind == TypeKind::POINTER
      && (from->arg->kind == TypeKind::VOID || to->arg->kind == TypeKind::VOID)) {
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

  // pointers to array can be casted to pointers to the first element.
  if (from->kind == TypeKind::POINTER && from->arg->kind == TypeKind::ARRAY
      && to->kind == TypeKind::POINTER
      && typeEq(from->arg->arg, to->arg)) {
    return 1;
  }

  if (from->kind == TypeKind::STRUCT && to->kind == TypeKind::UNION) {
    let unionDecl = lookupType(state, to->tag);
    if (unionDecl == NULL) {
      failSemaExpr(expr, "Can't find union decl to cast to");
    }

    let idx = 0;
    let structDecl = findTypeIdx(unionDecl->subTypes, from->tag, &idx);
    if (structDecl == NULL) {
      failSemaExpr(expr, "No way to convert struct to unrelated union");
    }
    castExpr->value = idx;
    return 1;
  }

  if (from->kind == TypeKind::POINTER && from->arg->kind == TypeKind::UNION
      && to->kind == TypeKind::POINTER && to->arg->kind == TypeKind::STRUCT) {
    let unionDecl = lookupType(state, from->arg->tag);
    if (unionDecl == NULL) {
      failSemaExpr(expr, "Can't find union decl to cast from");
    }

    let idx = 0;
    let structDecl = findTypeIdx(unionDecl->subTypes, to->arg->tag, &idx);
    if (structDecl == NULL) {
      failSemaExpr(expr, "No way to convert union to unrelated struct");
    }

    castExpr->value = idx;
    return 1;
  }

  return 0;
}


// Decays pointers to arrays to pointers to the first element
func doDecay(type: Type*) -> Type* {
  if (type->kind == TypeKind::POINTER && type->arg->kind == TypeKind::ARRAY) {
    let res = newType(TypeKind::POINTER);
    res->arg = type->arg->arg;
    return res;
  }

  return type;
}

func checkBool(expr: ExprAST*) {
  if (expr->type->kind != TypeKind::INT) {
    failSemaExpr(expr, ": Expected bool!");
  }
}

func findLocal(local: DeclList*, name: Token) -> DeclAST* {
  for (; local != NULL; local = local->next) {
    if (tokCmp(name, local->decl->name)) {
      return local->decl;
    }
  }
  return NULL;
}

func lookupLocal(state: SemaState*, name: Token) -> DeclAST* {
  for (; state != NULL; state = state->parent) {
    let local = findLocal(state->locals, name);
    if (local != NULL) {
      return local;
    }
  }

  return NULL;
}


// clang-format off
func findField(
    structDecl: DeclAST*,
    name: Token,
    idxOut: i32*
) -> DeclAST* {
  // clang-format on
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

func getSize(state: SemaState*, type: Type*) -> i32;

func getStructDeclSize(state: SemaState*, decl: DeclAST*) -> i32 {
  let size = 0;
  for (let field = decl->fields; field != NULL; field = field->next) {
    size += getSize(state, field->type);
  }
  return size == 0 ? 1 : size;
}

func getSize(state: SemaState*, type: Type*) -> i32 {
  switch (type->kind) {
    case TypeKind::VOID:
      return 0;

    // default enum is i32 = 4 bytes.
    case TypeKind::ENUM:
      return 4;

    case TypeKind::INT:
      return type->size / 8;

    case TypeKind::POINTER, TypeKind::FUNC:
      return 8;

    case TypeKind::ARRAY:
      if (type->size < 0) {
        failSema(SourceLoc{}, "Unsized array in sizeof");
      }
      return type->size * getSize(state, type->arg);

    // TODO: padding
    case TypeKind::STRUCT:
      let decl = lookupType(state, type->tag);
      if (decl == NULL) {
        failSema(SourceLoc{}, "Unkown type to get size of");
      }

      return getStructDeclSize(state, decl);

    default:
      failSema(SourceLoc{}, "Unknown type for size");
      return 0;
  }
}

func semaExpr(state: SemaState*, expr: ExprAST*);

func semaBinExpr(state: SemaState*, expr: ExprAST*) {
  semaExpr(state, expr->lhs);
  semaExpr(state, expr->rhs);

  // Handle special cases
  switch (expr->op.kind) {
    case TokenKind::COMMA:
      expr->type = expr->rhs->type;
      return;

    // comparision results in i32.
    case TokenKind::LESS, TokenKind::GREATER, TokenKind::LE_OP, TokenKind::GE_OP,
         TokenKind::EQ_OP, TokenKind::NE_OP:
      if (!typeEq(expr->lhs->type, expr->rhs->type)) {
        let lhsConv = doConvert(state, expr->lhs, expr->rhs->type);

        if (lhsConv == NULL) {
          let rhsConv = doConvert(state, expr->rhs, expr->lhs->type);
          if (rhsConv == NULL) {
            failSemaExpr(expr, ": Binary op on different types");
          }
          expr->rhs = rhsConv;
        } else {
          expr->lhs = lhsConv;
        }
      }
      expr->type = getInt32();
      return;

    case TokenKind::MINUS:
      if (expr->lhs->type->kind == TypeKind::POINTER
          && expr->rhs->type->kind == TypeKind::POINTER) {
        if (expr->lhs->type->arg->kind != TypeKind::INT
            || expr->lhs->type->arg->size != 8
            || expr->rhs->type->arg->kind != TypeKind::INT
            || expr->rhs->type->arg->size != 8) {
          // TODO: emit (expr) / sizeof(type)
          failSemaExpr(expr, "Only char pointer subtract supported");
        }

        expr->type = getIPtr();
        return;
      }
      if (expr->lhs->type->kind == TypeKind::POINTER
          && expr->rhs->type->kind == TypeKind::INT) {
        expr->type = expr->lhs->type;
        return;
      }
    case TokenKind::PLUS:
      if (expr->lhs->type->kind == TypeKind::INT
          && expr->rhs->type->kind == TypeKind::POINTER) {
        expr->type = expr->rhs->type;
        return;
      }
      if (expr->lhs->type->kind == TypeKind::POINTER
          && expr->rhs->type->kind == TypeKind::INT) {
        expr->type = expr->lhs->type;
        return;
      }
    case TokenKind::ADD_ASSIGN, TokenKind::SUB_ASSIGN:
      if (expr->lhs->type->kind == TypeKind::POINTER
          && expr->rhs->type->kind == TypeKind::INT) {
        expr->type = expr->lhs->type;
        return;
      }
    default:
      break;
  }

  if (isAssign(expr->op)) {
    let conv = doConvert(state, expr->rhs, expr->lhs->type);
    if (conv == NULL) {
      failSemaExpr(expr, ": Assign doesn't match");
    }
    expr->rhs = conv;
    expr->type = expr->lhs->type;
    return;
  }

  if (!typeEq(expr->lhs->type, expr->rhs->type)) {
    let lhsConv = doConvert(state, expr->lhs, expr->rhs->type);
    if (lhsConv == NULL) {
      let rhsConv = doConvert(state, expr->rhs, expr->lhs->type);
      if (rhsConv == NULL) {
        failSemaExpr(expr, ": type mismatch");
      }
      expr->rhs = rhsConv;
    } else {
      expr->lhs = lhsConv;
    }
  }

  expr->type = expr->lhs->type;
}

func getStringLength(tok: Token) -> i32 {
  let len = 0;

  for (let c = tok.data; c < tok.end; c++) {
    if (*c == 92) {
      c++;
      len++;
    } else {
      len++;
    }
  }

  return len + 1;  // null terminator
}

func semaStringType(state: SemaState*, expr: ExprAST*) {
  expr->type = newType(TypeKind::ARRAY);
  expr->type->arg = getCharType();
  expr->type->size = getStringLength(expr->identifier);
  expr->type->isConst = 1;
}

func semaString(state: SemaState*, expr: ExprAST*) {
  semaStringType(state, expr);

  // Add a global variable for the string.
  let root = getRoot(state);
  let decl = newDecl(DeclKind::VAR);
  decl->type = expr->type;

  let name: i8* = malloc(32 as u64);
  let n = sprintf(name, "str.%d", root->strCount++);
  decl->name.kind = TokenKind::IDENTIFIER;
  decl->name.data = name;
  decl->name.end = name + n;

  decl->init = newExpr(ExprKind::STR);
  decl->init->identifier = expr->identifier;
  decl->init->type = expr->type;

  decl->next = root->extraDecls;
  root->extraDecls = decl;

  let ptrType = newType(TypeKind::POINTER);
  ptrType->arg = decl->type;
  expr->type = ptrType;

  // transmute expr into a address of expr.
  let varRef = newExpr(ExprKind::VARIABLE);
  varRef->identifier = decl->name;

  expr->kind = ExprKind::UNARY;
  expr->op.kind = TokenKind::AND;
  expr->rhs = varRef;
}


// Finds a struct type in the state. Correctly looks for unions tags as well.
func lookupStruct(state: SemaState*, type: Type*) -> DeclAST* {
  // Check for union as parent
  if (type->arg != NULL) {
    let unionDecl = lookupType(state, type->arg->tag);
    if (unionDecl == NULL) {
      return NULL;
    }

    return findType(unionDecl->subTypes, type->tag);
  }
  return lookupType(state, type->tag);
}

func semaExpr(state: SemaState*, expr: ExprAST*) {
  switch (expr->kind) {
    case ExprKind::ARG_LIST:
      failSemaExpr(expr, "Arg list shouldn't occur");

    case ExprKind::STRUCT:
      let typeDecl: DeclAST* = NULL;
      if (expr->parent.kind != TokenKind::TOK_EOF) {
        let parentType = lookupType(state, expr->parent);
        if (parentType == NULL || parentType->type->kind != TypeKind::UNION) {
          failSemaExpr(expr, "Expected uninion type");
        }
        typeDecl = findType(parentType->subTypes, expr->identifier);
      } else {
        typeDecl = lookupType(state, expr->identifier);
      }
      if (typeDecl == NULL || typeDecl->type->kind != TypeKind::STRUCT) {
        failSemaExpr(expr, "Expected struct type for struct init expression");
      }

      // TODO: verify field completeness.
      for (let field = expr->rhs; field != NULL; field = field->rhs) {
        let fieldDecl = findField(typeDecl, field->identifier, &field->value);
        if (fieldDecl == NULL) {
          failSemaExpr(field, " cannot find field");
        }

        semaExpr(state, field->lhs);
        let conv = doConvert(state, field->lhs, fieldDecl->type);
        if (conv == NULL) {
          printType(field->lhs->type);
          printf("\n");
          printType(fieldDecl->type);
          failSemaExpr(field->lhs, "cannot convert to field type");
        }
        field->lhs = conv;
      }

      expr->type = typeDecl->type;

    case ExprKind::SCOPE:
      let decl = lookupType(state, expr->parent);
      if (decl == NULL || decl->type->kind != TypeKind::ENUM) {
        failSemaExpr(expr, "Expected enum type for scope expr");
      }

      let fieldDecl = findField(decl, expr->identifier, &expr->value);
      if (fieldDecl == NULL) {
        failSemaExpr(expr, " Cannot find field");
      }

      expr->type = newType(TypeKind::ENUM);
      expr->type->tag = decl->type->tag;

    case ExprKind::MEMBER:
      semaExpr(state, expr->lhs);

      let structDecl = NULL;
      if (expr->op.kind == TokenKind::PTR_OP) {
        if (expr->lhs->type->kind != TypeKind::POINTER
            || expr->lhs->type->arg->kind != TypeKind::STRUCT) {
          failSemaExpr(expr, ": Expected pointer to struct type for -> expr");
        }
        structDecl = lookupStruct(state, expr->lhs->type->arg);
      } else if (expr->op.kind == TokenKind::DOT) {
        if (expr->lhs->type->kind != TypeKind::STRUCT) {
          failSemaExpr(expr, "Expected struct type for . expr");
        }
        structDecl = lookupStruct(state, expr->lhs->type);
      } else {
        failSemaExpr(expr, "Unknown member op");
      }

      if (structDecl == NULL) {
        failSemaExpr(expr, "Unkown type for member expression");
      }

      let fieldDecl = findField(structDecl, expr->identifier, &expr->value);
      if (fieldDecl == NULL) {
        failSemaExpr(expr, " Cannot find field");
      }

      expr->type = fieldDecl->type;
    case ExprKind::CALL:
      semaExpr(state, expr->lhs);

      // We don't support function pointers
      if (expr->lhs->type->kind != TypeKind::FUNC) {
        failSemaExpr(expr, "Must call function type");
      }
      let curArgTy = expr->lhs->type->arg;
      let cur = expr->rhs;
      for (; cur != NULL; cur = cur->rhs) {
        semaExpr(state, cur->lhs);

        if (curArgTy != NULL) {
          let conv = doConvert(state, cur->lhs, curArgTy);
          if (conv == NULL) {
            printType(curArgTy);
            failSemaExpr(expr, " Arg type mismatch");
          }
          cur->lhs = conv;
        }
        if (curArgTy != NULL) {
          curArgTy = curArgTy->argNext;
        }
      }
      let isValidVararg = expr->lhs->type->isVarargs && curArgTy == NULL;
      if (!isValidVararg && (curArgTy == NULL) != (cur == NULL)) {
        failSemaExpr(expr, "Function call arg length mismatch");
      }
      expr->type = expr->lhs->type->result;
    case ExprKind::CONDITIONAL:
      semaExpr(state, expr->cond);
      checkBool(expr->cond);
      semaExpr(state, expr->lhs);
      semaExpr(state, expr->rhs);

      if (!typeEq(expr->lhs->type, expr->rhs->type)) {
        failSemaExpr(expr, "?: lhs and rhs should have same type");
      }
      expr->type = expr->lhs->type;
    case ExprKind::ARRAY:
      expr->type = newType(TypeKind::ARRAY);
      for (let sub = expr; sub != NULL; sub = sub->rhs) {
        semaExpr(state, sub->lhs);

        // Decay types in arrays.
        sub->lhs->type = doDecay(sub->lhs->type);

        if (expr->type->arg == NULL) {
          expr->type->arg = sub->lhs->type;
        } else if (!typeEq(expr->type->arg, sub->lhs->type)) {
          failSemaExpr(expr, "Init must have consistent type");
        }

        expr->type->size++;
      }

    case ExprKind::STR:
      semaString(state, expr);

    case ExprKind::VARIABLE:
      let local = lookupLocal(state, expr->identifier);
      if (local == NULL) {
        failSemaExpr(expr, "Couldn't find variable in scope");
      }

      // enum value, transform this expr to an i32.
      if (local->kind == DeclKind::ENUM_FIELD) {
        expr->kind = ExprKind::INT;
        expr->value = local->enumValue;
      }

      expr->type = local->type;

    case ExprKind::INT:
      expr->type = getInt32();
      return;

    case ExprKind::BINARY:
      semaBinExpr(state, expr);
    case ExprKind::INDEX:
      semaExpr(state, expr->lhs);
      if (expr->lhs->type->kind == TypeKind::POINTER
          && expr->lhs->type->arg->kind == TypeKind::ARRAY) {
        // auto insert deref to turn expr into an array.
        let derefExpr = newExpr(ExprKind::UNARY);
        derefExpr->op.kind = TokenKind::STAR;
        derefExpr->rhs = expr->lhs;
        derefExpr->type = expr->lhs->type->arg;

        expr->lhs = derefExpr;
      } else if (expr->lhs->type->kind != TypeKind::ARRAY) {
        failSemaExpr(expr, " Index only works on arrays, or pointers to them.");
      }
      semaExpr(state, expr->rhs);
      if (expr->rhs->type->kind != TypeKind::INT) {
        failSemaExpr(expr, "Can't index with non integer");
      }
      expr->type = expr->lhs->type->arg;
    case ExprKind::UNARY:
      if (expr->op.kind == TokenKind::AND) {
        semaExpr(state, expr->rhs);
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
          failSemaExpr(expr, "Expected pointer type for *");
        }
        expr->type = expr->type->arg;
      }

      // TODO: correct?
      if (expr->op.kind == TokenKind::BANG) {
        expr->type = getInt32();
      }

    case ExprKind::SIZEOF:
      if (expr->rhs != NULL) {
        semaExpr(state, expr->rhs);
        expr->value = getSize(state, expr->rhs->type);
      } else {
        expr->value = getSize(state, expr->sizeofArg);
      }
      expr->kind = ExprKind::INT;
      expr->type = getUPtr();
    case ExprKind::CAST:
      semaExpr(state, expr->lhs);
      if (!semaCast(state, expr)) {
        printType(expr->lhs->type);
        printf(" -> ");
        printType(expr->type);
        failSemaExpr(expr, " Can't cast");
      }
    case ExprKind::PAREN:
      semaExpr(state, expr->lhs);
      expr->type = expr->lhs->type;
  }
}

func semaStmt(state: SemaState*, stmt: StmtAST*);

func semaTopLevel(state: SemaState*, decl: DeclAST*) -> DeclAST*;

func addLocalDecl(state: SemaState*, decl: DeclAST*) {
  let prev = findLocal(state->locals, decl->name);
  if (prev != NULL) {
    // Allow redef of functions, TODO: verify type match...
    if (prev->kind == DeclKind::FUNC && prev->body == NULL) {
      prev->hasDef = 1;
    } else {
      failSemaDecl(decl, "Variable redef");
    }
  }

  let newLocal = newDeclList(decl);
  newLocal->next = state->locals;
  state->locals = newLocal;
}


// TODO: do this on 'doConvert'?
func sizeArrayTypes(declType: Type*, initType: Type*) {
  if (declType->kind == TypeKind::ARRAY && declType->size < 0) {
    declType->size = initType->size;
    if (declType->size < 0) {
      failSema(SourceLoc{}, "Coudln't infer array size");
    }
  }

  // recurse into pointers and arrays
  if (declType->kind == TypeKind::POINTER
      || declType->kind == TypeKind::ARRAY) {
    sizeArrayTypes(declType->arg, initType->arg);
  }
}

func semaDecl(state: SemaState*, decl: DeclAST*) {
  resolveTypeTags(state, decl->type);

  switch (decl->kind) {
    case DeclKind::STRUCT:
      // Resolve tags in fields.
      for (let field = decl->fields; field != NULL; field = field->next) {
        if (field->kind != DeclKind::VAR) {
          failSemaDecl(field, "Only var decls allowed in struct");
        }

        resolveTypeTags(state, field->type);
      }
    case DeclKind::ENUM:
      break;
    case DeclKind::UNION:
      let maxSize = 0;
      for (let tag = decl->subTypes; tag != NULL; tag = tag->next) {
        // sema the 'tag' which is a struct.
        semaDecl(state, tag->decl);

        let size = getStructDeclSize(state, tag->decl);
        if (size > maxSize) {
          maxSize = size;
        }
      }
      decl->enumValue = maxSize;
      break;
    case DeclKind::FUNC:
      addLocalDecl(state, decl);
      if (decl->body != NULL) {
        let funcState: SemaState = { 0,};
        funcState.parent = state;
        funcState.result = decl->type->result;

        // Generate a local for each arg.
        for (let field = decl->fields; field != NULL; field = field->next) {
          resolveTypeTags(state, field->type);
          addLocalDecl(&funcState, field);
        }
        semaStmt(&funcState, decl->body);
      }
    case DeclKind::VAR:
      addLocalDecl(state, decl);
      if (decl->init != NULL) {
        semaExpr(state, decl->init);

        if (decl->type == NULL) {
          decl->type = decl->init->type;
        } else if (decl->type->kind == TypeKind::STRUCT
            && decl->init->kind == ExprKind::ARRAY) {
          // TODO: verify match?
          if (decl->init->rhs != NULL || decl->init->lhs->kind != ExprKind::INT) {
            failSemaDecl(decl, "Currently only zero init supported");
          }
          decl->init->type = decl->type;
        } else if (decl->init = doConvert(state, decl->init, decl->type),
            decl->init == NULL) {
          failSemaDecl(decl, ": Decl init type doesn't match");
        }

        sizeArrayTypes(decl->type, decl->init->type);
      }
    case DeclKind::IMPORT:
      if (state->parent != NULL) {
        failSemaDecl(decl, "Import not allowed in local scope");
      }

      // Check if we already import this one
      for (let cur = state->imports; cur != NULL; cur = cur->next) {
        if (tokCmp(decl->name, cur->name)) {
          return;
        }
      }

      // Add to imports
      let imports: ImportList* = calloc(1, sizeof(struct ImportList));
      imports->name = decl->name;
      imports->next = state->imports;
      state->imports = imports;

      let buf: i8* = malloc(256);
      sprintf(buf, "%.*s.b", decl->name.end - decl->name.data, decl->name.data);
      let fileDecls = parseFile(buf);
      if (fileDecls == NULL) {
        failSemaDecl(decl, "Failed to import file");
      }

      // semaTopLevel will return a combined list of decls from the file and the
      // extraDecls.
      let extras = semaTopLevel(state, fileDecls);
      state->extraDecls = extras;

    case DeclKind::ENUM_FIELD:
      failSemaDecl(decl, "Shoudln't happen");
      return;
  }
}

func newState(parent: SemaState*) -> SemaState {
  let state: SemaState = { 0,};
  state.parent = parent;
  state.result = parent->result;
  return state;
}

func getFieldCount(decl: DeclAST*) -> i32 {
  let count = 0;

  if (decl->kind == DeclKind::ENUM) {
    for (let field = decl->fields; field != NULL; field = field->next) {
      count++;
    }
  } else if (decl->kind == DeclKind::UNION) {
    for (let tag = decl->subTypes; tag != NULL; tag = tag->next) {
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
      if (unionDecl == NULL) {
        failSemaExpr(expr, "Unknown union");
      }

      let tagIdx = 0;
      let tagDecl = findTypeIdx(unionDecl->subTypes, tagName, &tagIdx);
      if (tagDecl == NULL) {
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
      if (decl == NULL) {
        failSemaExpr(expr, "Couldn't find type");
      }

      if (decl->type->kind == TypeKind::ENUM) {
        let fieldDecl = findField(decl, expr->identifier, &expr->value);
        if (fieldDecl == NULL) {
          failSemaExpr(expr, " Cannot find field");
        }

        expr->type = newType(TypeKind::ENUM);
        expr->type->tag = decl->type->tag;
      } else if (decl->type->kind == TypeKind::UNION) {
        let tagDecl = findTypeIdx(
            decl->subTypes,
            expr->identifier,
            &expr->value);
        if (tagDecl == NULL) {
          failSemaExpr(expr, "Cannot find tag");
        }

        expr->type = decl->type;
      } else {
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
      switchType->kind == TypeKind::ENUM
      || switchType->kind == TypeKind::UNION;

  if (switchType->kind != TypeKind::INT && !exhaustive) {
    printType(stmt->expr->type);
    failSemaExpr(stmt->expr, "Switch expr must be integer, enum or union");
  }

  let fieldBitSet = 0;

  for (let caseStmt = stmt->stmt; caseStmt != NULL;
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

    for (let cur = caseStmt->stmt; cur != NULL; cur = cur->nextStmt) {
      semaStmt(&subState, cur);
    }
  }

  if (exhaustive && fieldBitSet != -1) {
    let typeTag = switchType->tag;
    let decl = lookupType(state, typeTag);
    if (decl == NULL) {
      failSemaStmt(stmt, "Couldn't find enum decl");
    }
    let size = getFieldCount(decl);
    if ((1 << size) - 1 != fieldBitSet) {
      failSemaStmt(stmt, "Switch is not exhaustive");
    }
  }
}

func semaStmt(state: SemaState*, stmt: StmtAST*) {
  switch (stmt->kind) {
    case StmtKind::EXPR:
      if (stmt->expr != NULL) {
        semaExpr(state, stmt->expr);
      }
    case StmtKind::DECL:
      if (stmt->decl->kind != DeclKind::VAR) {
        failSemaStmt(stmt, "Only var decls allowed in local scope");
      }
      return semaDecl(state, stmt->decl);

    case StmtKind::RETURN:
      if (stmt->expr == NULL && state->result->kind != TypeKind::VOID) {
        failSemaStmt(stmt, "Return type should be void");
      }
      if (stmt->expr != NULL) {
        semaExpr(state, stmt->expr);
        let conv = doConvert(state, stmt->expr, state->result);
        if (conv == NULL) {
          failSemaStmt(stmt, "Return type mismatch");
        }
        stmt->expr = conv;
      }
    case StmtKind::COMPOUND:
      let subState = newState(state);

      for (let cur = stmt->stmt; cur != NULL; cur = cur->nextStmt) {
        semaStmt(&subState, cur);
      }

    case StmtKind::IF:
      semaExpr(state, stmt->expr);
      checkBool(stmt->expr);

      semaStmt(state, stmt->init);
      if (stmt->stmt != NULL) {
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

func addTaggedType(state: SemaState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::STRUCT, DeclKind::ENUM:
      if (findType(state->types, decl->type->tag) != NULL) {
        failSemaDecl(decl, ": Type redef");
      }

      // Add the struct to the types.
      let type = newDeclList(decl);
      type->next = state->types;
      state->types = type;
    case DeclKind::UNION:
      if (findType(state->types, decl->type->tag) != NULL) {
        failSemaDecl(decl, ": Type redef");
      }

      let type = newDeclList(decl);
      type->next = state->types;
      state->types = type;

      // TODO: Add a namespaced struct type for each nested tag
      break;
    default:
      break;
  }
}

func semaTopLevel(state: SemaState*, decl: DeclAST*) -> DeclAST* {
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
  let semaState: SemaState = { 0,};
  let nullTok: Token;
  nullTok.kind = TokenKind::IDENTIFIER;
  nullTok.data = "NULL";
  nullTok.end = nullTok.data + 4;

  // Add null as a nullptr
  let nullDecl = newDecl(DeclKind::ENUM_FIELD);
  nullDecl->name = nullTok;
  nullDecl->enumValue = 0;
  nullDecl->type = newType(TypeKind::POINTER);
  nullDecl->type->arg = newType(TypeKind::VOID);

  semaState.locals = newDeclList(nullDecl);
  return semaState;
}
