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
  while (state->parent != null) {
    state = state->parent;
  }
  return state;
}

func typeEq(one: Type*, two: Type*) -> i32 {
  switch (one->kind) {
    case TypeKind::Void:
      return &two->kind as TypeKind::Void* != null;
    case TypeKind::Int as int1:
      let int2 = &two->kind as TypeKind::Int*;
      if (int2 == null) {
        return 0;
      }
      return int1.isSigned == int2->isSigned && int1.size == int2->size;

    case TypeKind::Array as ar1:
      let ar2 = &two->kind as TypeKind::Array*;
      if (ar2 == null
          || (ar1.size >= 0 && ar2->size >= 0 && ar1.size != ar2->size)) {
        return 0;
      }
      return typeEq(ar1.element, ar2->element);
    case TypeKind::Pointer as ptr1:
      let ptr2 = &two->kind as TypeKind::Pointer*;
      if (ptr2 == null) {
        return 0;
      }
      return typeEq(ptr1.pointee, ptr2->pointee);

    case TypeKind::Struct as s1:
      let s2 = &two->kind as TypeKind::Struct*;
      if (s2 == null) {
        return 0;
      }
      if (s1.parent != null) {
        if (s2->parent == null || !typeEq(s1.parent, s2->parent)) {
          return 0;
        }
      }
      return tokCmp(s1.tag, s2->tag);
    case TypeKind::Enum as e1:
      let e2 = &two->kind as TypeKind::Enum*;
      if (e2 == null) {
        return 0;
      }
      return tokCmp(e1.tag, e2->tag);
    case TypeKind::Union as u1:
      let u2 = &two->kind as TypeKind::Union*;
      if (u2 == null) {
        return 0;
      }
      return tokCmp(u1.tag, u2->tag);

    case TypeKind::Func:
      failSema(SourceLoc {}, "TODO: type eq func");
    case TypeKind::Tag:
      failSema(SourceLoc {}, "Type tag not resolved before eq");
  }

  return 1;
}

func getTypeTag(type: Type*) -> Token* {
  switch (type->kind) {
    case TypeKind::Struct as s:
      return &s.tag;
    case TypeKind::Union as u:
      return &u.tag;
    case TypeKind::Enum as e:
      return &e.tag;
    case TypeKind::Tag:
      failSema(SourceLoc {}, "Tags not resolved!");
    default:
      return null;
  }
}

func findTypeIdx(types: DeclList*, tag: Token, idxOut: i32*) -> DeclAST* {
  let idx = 0;
  for (; types != null; types = types->next, idx++) {
    let typeTag = getTypeTag(types->decl->type);
    if (typeTag != null && tokCmp(tag, *typeTag)) {
      if (idxOut != null) {
        *idxOut = idx;
      }
      return types->decl;
    }
  }
  return null;
}

func findType(types: DeclList*, tag: Token) -> DeclAST* {
  return findTypeIdx(types, tag, null);
}

func lookupType(state: SemaState*, tag: Token) -> DeclAST* {
  for (; state != null; state = state->parent) {
    let type = findType(state->types, tag);
    if (type != null) {
      return type;
    }
  }

  return null;
}

func doConvert(state: SemaState*, expr: ExprAST*, to: Type*) -> ExprAST* {
  let from = expr->type;

  if (typeEq(from, to)) {
    return expr;
  }

  switch (to->kind) {
    // Allow integer expression casting
    case TypeKind::Int:
      if (expr->kind == ExprKind::INT) {
        let res = newExpr(ExprKind::INT);
        res->location = expr->location;
        res->value = expr->value;
        res->type = to;
        return res;
      }

    case TypeKind::Pointer as toPtr:
      let fromPtr = &from->kind as TypeKind::Pointer*;
      if (fromPtr == null) {
        break;
      }

      // TODO: Remove and use 'as' once the ': type' syntax is implemented.
      //
      // void * can be converted from and to any other pointer..
      if (&fromPtr->pointee->kind as TypeKind::Void* != null
          || &toPtr.pointee->kind as TypeKind::Void* != null) {
        return expr;
      }

      // Pointer to arrays can be convert to pointers to the first element.
      // This is a no-op for code gen?
      let fromArray = &fromPtr->pointee->kind as TypeKind::Array*;
      if (fromArray != null && typeEq(fromArray->element, toPtr.pointee)) {
        return expr;
      }

    // A union member can be converted to the union type by inserting the kind.
    case TypeKind::Union as toUnion:
      let fromStruct = &from->kind as TypeKind::Struct*;
      if (fromStruct == null) {
        break;
      }
      let unionDecl = lookupType(state, toUnion.tag);
      if (unionDecl == null) {
        failSemaExpr(expr, "Can't find union decl to cast to");
      }

      let idx = 0;
      let structDecl = findTypeIdx(unionDecl->subTypes, fromStruct->tag, &idx);
      if (structDecl == null) {
        failSemaExpr(expr, "No way to convert struct to unrelated union");
      }

      let castExpr = newExpr(ExprKind::CAST);
      castExpr->location = expr->location;
      castExpr->lhs = expr;
      castExpr->type = to;
      castExpr->value = idx;
      castExpr->castKind = CastKind::StructUnion;
      return castExpr;

    default:
      break;
  }

  return null;
}

func resolveTypeTags(state: SemaState*, type: Type*) {
  if (type == null) {
    return;
  }

  switch (type->kind) {
    case TypeKind::Tag as tagType:
      if (tagType.parent.kind != TokenKind::TOK_EOF) {
        let parentDecl = lookupType(state, tagType.parent);
        if (parentDecl == null) {
          failSema(SourceLoc {}, "Can't resolve type tags, unknown parent type");
        }

        let tagDecl = findType(parentDecl->subTypes, tagType.tag);
        if (tagDecl == null) {
          failSema(SourceLoc {}, "Can't resolve type tags, unknown sub type");
        }

        *type = *tagDecl->type;
      } else {
        let typeDecl = lookupType(state, tagType.tag);
        if (typeDecl == null) {
          failSema(SourceLoc {}, "Can't resolve type tags, unknown type");
        }
        type->kind = typeDecl->type->kind;
      }

    case TypeKind::Pointer as p:
      resolveTypeTags(state, p.pointee);
    case TypeKind::Array as a:
      resolveTypeTags(state, a.element);
    case TypeKind::Func as f:
      resolveTypeTags(state, f.result);
      resolveTypeTags(state, f.args);

    // TODO: is struct parent needed?
    default:
      break;
  }

  resolveTypeTags(state, type->next);
}

func semaIntCast(
    expr: ExprAST*,
    fromInt: TypeKind::Int*,
    toInt: TypeKind::Int*
) -> i32 {
  // Sign change, no-op for now.
  if (fromInt->size == toInt->size) {
    expr->castKind = CastKind::Noop;
    return 1;
  }

  // Same signedness but different type
  if (fromInt->size > toInt->size) {
    expr->castKind = CastKind::Trunc;
    return 1;
  }

  if (fromInt->isSigned && toInt->isSigned) {
    expr->castKind = CastKind::Sext;
    return 1;
  }

  if (!fromInt->isSigned && !toInt->isSigned) {
    expr->castKind = CastKind::Zext;
    return 1;
  }

  return 0;
}

func semaCast(state: SemaState*, castExpr: ExprAST*) -> i32 {
  resolveTypeTags(state, castExpr->type);
  if (castExpr->type == null) {
    failSemaExpr(castExpr, "Cast without type?");
  }

  let to = castExpr->type;
  let expr = castExpr->lhs;
  let from = expr->type;

  if (typeEq(from, to)) {
    castExpr->castKind = CastKind::Noop;
    return 1;
  }

  // Allow casting int expressions '64 as i32'
  if (expr->kind == ExprKind::INT) {
    castExpr->castKind = CastKind::Noop;
    expr->type = to;
    return 1;
  }

  switch (from->kind) {
    case TypeKind::Int as fromInt:
      let toInt = &to->kind as TypeKind::Int*;
      if (toInt != null && semaIntCast(castExpr, &fromInt, toInt)) {
        return 1;
      }

      // enums can be casted from integers
      if (&to->kind as TypeKind::Enum* != null) {
        let enumInt = TypeKind::Int {
          size = 32,
          isSigned = 1,
        };
        return semaIntCast(castExpr, &fromInt, &enumInt);
      }

    case TypeKind::Enum:
      // enums can be casted to integers
      let toInt = &to->kind as TypeKind::Int*;
      if (toInt != null) {
        let enumInt = TypeKind::Int {
          size = 32,
          isSigned = 1,
        };
        return semaIntCast(castExpr, &enumInt, toInt);

        return 1;
      }

    case TypeKind::Pointer as fromPtr:
      let toPtr = &to->kind as TypeKind::Pointer*;
      if (toPtr == null) {
        break;
      }

      // void * can be casted from and to any other pointer..
      if (&fromPtr.pointee->kind as TypeKind::Void* != null
          || &toPtr->pointee->kind as TypeKind::Void* != null) {
        castExpr->castKind = CastKind::Noop;
        return 1;
      }

      // Pointer to arrays can be casted to pointers to the first element.
      // This is a no-op for code gen?
      let fromArray = &fromPtr.pointee->kind as TypeKind::Array*;
      if (fromArray != null && typeEq(fromArray->element, toPtr->pointee)) {
        castExpr->castKind = CastKind::Noop;
        return 1;
      }

      // Pointers to unions can be converted to pointers to structs.
      let fromUnion = &fromPtr.pointee->kind as TypeKind::Union*;
      let toStruct = &toPtr->pointee->kind as TypeKind::Struct*;
      if (fromUnion != null && toStruct != null) {
        let unionDecl = lookupType(state, fromUnion->tag);
        if (unionDecl == null) {
          failSemaExpr(expr, "Can't find union decl to cast from");
        }

        let idx = 0;
        let structDecl = findTypeIdx(unionDecl->subTypes, toStruct->tag, &idx);
        if (structDecl == null) {
          failSemaExpr(expr, "No way to convert union to unrelated struct");
        }

        castExpr->value = idx;
        castExpr->castKind = CastKind::UnionStructPtr;
        return 1;
      }

    case TypeKind::Struct as fromStruct:
      let toUnion = &to->kind as TypeKind::Union*;
      if (toUnion == null) {
        break;
      }
      let unionDecl = lookupType(state, toUnion->tag);
      if (unionDecl == null) {
        failSemaExpr(expr, "Can't find union decl to cast to");
      }

      let idx = 0;
      let structDecl = findTypeIdx(unionDecl->subTypes, fromStruct.tag, &idx);
      if (structDecl == null) {
        failSemaExpr(expr, "No way to convert struct to unrelated union");
      }

      castExpr->value = idx;
      castExpr->castKind == CastKind::StructUnion;
      return 1;

    default:
      break;
  }

  return 0;
}


func getPointerToArray(type: Type*) -> TypeKind::Array* {
  let fromPtr = &type->kind as TypeKind::Pointer*;
  if (fromPtr == null) {
    return null;
  }
  let fromArray = &fromPtr->pointee->kind as TypeKind::Array*;
  if (fromArray == null) {
    return null;
  }
  return fromArray;
}


// Decays pointers to arrays to pointers to the first element
func doDecay(type: Type*) -> Type* {
  let fromArray = getPointerToArray(type);
  if (fromArray == null) {
    return type;
  }

  let res = newType(TypeKind::Pointer {
    pointee = fromArray->element,
  });
  return res;
}

func checkBool(expr: ExprAST*) {
  if (&expr->type->kind as TypeKind::Int* == null) {
    failSemaExpr(expr, ": Expected bool!");
  }
}

func findLocal(local: DeclList*, name: Token) -> DeclAST* {
  for (; local != null; local = local->next) {
    if (tokCmp(name, local->decl->name)) {
      return local->decl;
    }
  }
  return null;
}

func lookupLocal(state: SemaState*, name: Token) -> DeclAST* {
  for (; state != null; state = state->parent) {
    let local = findLocal(state->locals, name);
    if (local != null) {
      return local;
    }
  }

  return null;
}


// clang-format off
func findField(
    structDecl: DeclAST*,
    name: Token,
    idxOut: i32*
) -> DeclAST* {
  // clang-format on
  let idx = 0;
  for (let field = structDecl->fields; field != null;
       field = field->next, idx++) {
    if (tokCmp(name, field->name)) {
      *idxOut = idx;
      return field;
    }
  }
  return null;
}

func getSize(state: SemaState*, type: Type*) -> i32;

func getStructDeclSize(state: SemaState*, decl: DeclAST*) -> i32 {
  let size = 0;
  for (let field = decl->fields; field != null; field = field->next) {
    size += getSize(state, field->type);
  }
  return size == 0 ? 1 : size;
}

func getSize(state: SemaState*, type: Type*) -> i32 {
  switch (type->kind) {
    case TypeKind::Void:
      return 0;

    // default enum is i32 = 4 bytes.
    case TypeKind::Enum:
      return 4;

    case TypeKind::Int as int:
      return int.size / 8;

    case TypeKind::Pointer:
      return 8;
    case TypeKind::Func:
      return 8;

    case TypeKind::Array as arr:
      if (arr.size < 0) {
        failSema(SourceLoc {}, "Unsized array in sizeof");
      }
      return arr.size * getSize(state, arr.element);

    // TODO: padding
    case TypeKind::Struct as s:
      let decl = lookupType(state, s.tag);
      if (decl == null) {
        failSema(SourceLoc {}, "Unkown type to get size of");
      }

      return getStructDeclSize(state, decl);

    case TypeKind::Union as u:
      let maxSize = 0;
      let decl = lookupType(state, u.tag);
      for (let sub = decl->subTypes; sub != null; sub = sub->next) {
        let size = getStructDeclSize(state, sub->decl);
        if (size > maxSize) {
          maxSize = size;
        }
      }
      return maxSize + 4;      // i32 tag.

    default:
      failSema(SourceLoc {}, "Unknown type for size");
      return 0;
  }
}

func semaExpr(state: SemaState*, expr: ExprAST*);

func semaBinExpr(state: SemaState*, expr: ExprAST*) {
  semaExpr(state, expr->lhs);
  semaExpr(state, expr->rhs);

  let lhsTypePtr = &expr->lhs->type->kind as TypeKind::Pointer*;
  let lhsTypeInt = &expr->lhs->type->kind as TypeKind::Int*;

  let rhsTypePtr = &expr->rhs->type->kind as TypeKind::Pointer*;
  let rhsTypeInt = &expr->rhs->type->kind as TypeKind::Int*;

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

        if (lhsConv == null) {
          let rhsConv = doConvert(state, expr->rhs, expr->lhs->type);
          if (rhsConv == null) {
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
      if (lhsTypePtr != null && rhsTypePtr != null) {
        if (!typeEq(lhsTypePtr->pointee, getCharType())
            || !typeEq(rhsTypePtr->pointee, lhsTypePtr->pointee)) {
          // TODO: emit (expr) / sizeof(type)
          failSemaExpr(expr, "Only char pointer subtract supported");
        }

        expr->type = getIPtr();
        return;
      }
      if (lhsTypePtr != null && rhsTypeInt != null) {
        expr->type = expr->lhs->type;
        return;
      }
    case TokenKind::PLUS:
      if (lhsTypeInt != null && rhsTypePtr != null) {
        expr->type = expr->rhs->type;
        return;
      }
      if (lhsTypePtr != null && rhsTypeInt != null) {
        expr->type = expr->lhs->type;
        return;
      }
    case TokenKind::ADD_ASSIGN, TokenKind::SUB_ASSIGN:
      if (lhsTypePtr != null && rhsTypeInt != null) {
        expr->type = expr->lhs->type;
        return;
      }
    default:
      break;
  }

  if (isAssign(expr->op)) {
    let conv = doConvert(state, expr->rhs, expr->lhs->type);
    if (conv == null) {
      failSemaExpr(expr, ": Assign doesn't match");
    }
    expr->rhs = conv;
    expr->type = expr->lhs->type;
    return;
  }

  if (!typeEq(expr->lhs->type, expr->rhs->type)) {
    let lhsConv = doConvert(state, expr->lhs, expr->rhs->type);
    if (lhsConv == null) {
      let rhsConv = doConvert(state, expr->rhs, expr->lhs->type);
      if (rhsConv == null) {
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
  expr->type = newType(TypeKind::Array {
    element = getCharType(),
    size = getStringLength(expr->identifier),
  });
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

  let ptrType = newType(TypeKind::Pointer {
    pointee = decl->type,
  });
  expr->type = ptrType;

  // transmute expr into a address of expr.
  let varRef = newExpr(ExprKind::VARIABLE);
  varRef->identifier = decl->name;

  expr->kind = ExprKind::UNARY;
  expr->op.kind = TokenKind::AND;
  expr->rhs = varRef;
}


// Finds a struct type in the state. Correctly looks for unions tags as well.
func lookupStruct(state: SemaState*, type: TypeKind::Struct*) -> DeclAST* {
  // Check for union as parent
  if (type->parent != null) {
    let tag = getTypeTag(type->parent);
    if (tag == null) {
      return null;
    }
    let unionDecl = lookupType(state, *tag);
    if (unionDecl == null) {
      return null;
    }

    return findType(unionDecl->subTypes, type->tag);
  }
  return lookupType(state, type->tag);
}

func semaDecl(state: SemaState*, decl: DeclAST*);

func semaExpr(state: SemaState*, expr: ExprAST*) {
  switch (expr->kind) {
    case ExprKind::ARG_LIST:
      failSemaExpr(expr, "Arg list shouldn't occur");

    case ExprKind::STRUCT:
      let typeDecl: DeclAST* = null;
      if (expr->parent.kind != TokenKind::TOK_EOF) {
        let parentDecl = lookupType(state, expr->parent);
        if (parentDecl == null || parentDecl->kind != DeclKind::UNION) {
          failSemaExpr(expr, "Expected uninion type");
        }
        typeDecl = findType(parentDecl->subTypes, expr->identifier);
      } else {
        typeDecl = lookupType(state, expr->identifier);
      }
      if (typeDecl == null || typeDecl->kind != DeclKind::STRUCT) {
        failSemaExpr(expr, "Expected struct type for struct init expression");
      }

      // TODO: verify field completeness.
      for (let field = expr->rhs; field != null; field = field->rhs) {
        let fieldDecl = findField(typeDecl, field->identifier, &field->value);
        if (fieldDecl == null) {
          failSemaExpr(field, " cannot find field");
        }

        semaExpr(state, field->lhs);
        let conv = doConvert(state, field->lhs, fieldDecl->type);
        if (conv == null) {
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
      if (decl == null || decl->kind != DeclKind::ENUM) {
        failSemaExpr(expr, "Expected enum type for scope expr");
      }

      let fieldDecl = findField(decl, expr->identifier, &expr->value);
      if (fieldDecl == null) {
        failSemaExpr(expr, " Cannot find field");
      }

      expr->type = decl->type;

    case ExprKind::MEMBER:
      semaExpr(state, expr->lhs);

      let structDecl = null;
      if (expr->op.kind == TokenKind::PTR_OP) {
        let ptrType = &expr->lhs->type->kind as TypeKind::Pointer*;
        let structType = ptrType == null
             ? null as TypeKind::Struct*
             : &ptrType->pointee->kind as TypeKind::Struct*;
        if (structType == null) {
          failSemaExpr(expr, ": Expected pointer to struct type for -> expr");
        }
        structDecl = lookupStruct(state, structType);
      } else if (expr->op.kind == TokenKind::DOT) {
        let structType = &expr->lhs->type->kind as TypeKind::Struct*;
        if (structType == null) {
          failSemaExpr(expr, "Expected struct type for . expr");
        }
        structDecl = lookupStruct(state, structType);
      } else {
        failSemaExpr(expr, "Unknown member op");
      }

      if (structDecl == null) {
        failSemaExpr(expr, "Unknown type for member expression");
      }

      let fieldDecl = findField(structDecl, expr->identifier, &expr->value);
      if (fieldDecl == null) {
        failSemaExpr(expr, " Cannot find field");
      }

      expr->type = fieldDecl->type;

    case ExprKind::CALL:
      semaExpr(state, expr->lhs);

      // We don't support function pointers
      let funType = &expr->lhs->type->kind as TypeKind::Func*;
      if (funType == null) {
        failSemaExpr(expr, "Must call function type");
      }
      let curArgTy = funType->args;
      let cur = expr->rhs;
      for (; cur != null; cur = cur->rhs) {
        semaExpr(state, cur->lhs);

        if (curArgTy != null) {
          let conv = doConvert(state, cur->lhs, curArgTy);
          if (conv == null) {
            printType(curArgTy);
            failSemaExpr(expr, " Arg type mismatch");
          }
          cur->lhs = conv;
        }
        if (curArgTy != null) {
          curArgTy = curArgTy->next;
        }
      }
      let isValidVararg = funType->isVarargs && curArgTy == null;
      if (!isValidVararg && (curArgTy == null) != (cur == null)) {
        failSemaExpr(expr, "Function call arg length mismatch");
      }
      expr->type = funType->result;

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
      let size = 0;
      let elementType: Type* = null;
      for (let sub = expr; sub != null; sub = sub->rhs) {
        semaExpr(state, sub->lhs);

        // Decay types in arrays.
        sub->lhs->type = doDecay(sub->lhs->type);

        if (elementType == null) {
          elementType = sub->lhs->type;
        } else if (!typeEq(elementType, sub->lhs->type)) {
          failSemaExpr(expr, "Init must have consistent type");
        }

        size++;
      }

      expr->type = newType(TypeKind::Array {
        size = size,
        element = elementType,
      });

    case ExprKind::STR:
      semaString(state, expr);

    case ExprKind::VARIABLE:
      let local = lookupLocal(state, expr->identifier);
      if (local == null) {
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

      let ptrToArray = getPointerToArray(expr->lhs->type);
      if (ptrToArray != null) {
        // auto insert deref to turn expr into an array.
        let derefExpr = newExpr(ExprKind::UNARY);
        derefExpr->op.kind = TokenKind::STAR;
        derefExpr->rhs = expr->lhs;
        derefExpr->type = newType(*ptrToArray);

        expr->lhs = derefExpr;
      }

      let array = &expr->lhs->type->kind as TypeKind::Array*;
      if (array == null) {
        failSemaExpr(expr, " Index only works on arrays, or pointers to them.");
      }

      semaExpr(state, expr->rhs);
      if (&expr->rhs->type->kind as TypeKind::Int* == null) {
        failSemaExpr(expr, "Can't index with non integer");
      }
      expr->type = array->element;

    case ExprKind::UNARY:
      if (expr->op.kind == TokenKind::AND) {
        semaExpr(state, expr->rhs);
        expr->type = newType(TypeKind::Pointer {
          pointee = expr->rhs->type,
        });
        return;
      }

      if (expr->lhs != null) {
        semaExpr(state, expr->lhs);
        expr->type = expr->lhs->type;
      } else {
        semaExpr(state, expr->rhs);
        expr->type = expr->rhs->type;
      }

      // Handle the specials
      if (expr->op.kind == TokenKind::STAR) {
        let ptrType = &expr->rhs->type->kind as TypeKind::Pointer*;
        if (ptrType == null) {
          failSemaExpr(expr, "Expected pointer type for *");
        }
        expr->type = ptrType->pointee;
      }

      // TODO: correct?
      if (expr->op.kind == TokenKind::BANG) {
        expr->type = getInt32();
      }

    case ExprKind::SIZEOF:
      if (expr->rhs != null) {
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

    case ExprKind::LET:
      if (expr->decl->kind != DeclKind::VAR) {
        failSemaExpr(expr, "Only let expressions allowed");
      }
      if (expr->decl->init == null) {
        failSemaExpr(expr, "Let expression must have an init");
      }
      semaDecl(state, expr->decl);
      expr->type = expr->decl->type;
  }
}

func semaStmt(state: SemaState*, stmt: StmtAST*);

func semaTopLevel(state: SemaState*, decl: DeclAST*) -> DeclAST*;

func addLocalDecl(state: SemaState*, decl: DeclAST*) {
  let prev = findLocal(state->locals, decl->name);
  if (prev != null) {
    // Allow redef of functions, TODO: verify type match...
    if (prev->kind == DeclKind::FUNC && prev->body == null) {
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
  switch (declType->kind) {
    case TypeKind::Array as array:
      let initArray = &initType->kind as TypeKind::Array*;
      array.size = initArray->size;
      if (array.size < 0) {
        failSema(SourceLoc {}, "Coudln't infer array size");
      }
      sizeArrayTypes(array.element, initArray->element);

    case TypeKind::Pointer as ptr:
      let ptrInit = &initType->kind as TypeKind::Pointer*;
      sizeArrayTypes(ptr.pointee, ptrInit->pointee);
    default:
      break;
  }
}

func semaDecl(state: SemaState*, decl: DeclAST*) {
  resolveTypeTags(state, decl->type);

  switch (decl->kind) {
    case DeclKind::STRUCT:
      // Resolve tags in fields.
      for (let field = decl->fields; field != null; field = field->next) {
        if (field->kind != DeclKind::VAR) {
          failSemaDecl(field, "Only var decls allowed in struct");
        }

        resolveTypeTags(state, field->type);
      }
    case DeclKind::ENUM:
      break;
    case DeclKind::UNION:
      let maxSize = 0;
      for (let tag = decl->subTypes; tag != null; tag = tag->next) {
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
      if (decl->body != null) {
        let funcState = SemaState {
          parent = state,
          result = (&decl->type->kind as TypeKind::Func*)->result,
        };

        // Generate a local for each arg.
        for (let field = decl->fields; field != null; field = field->next) {
          resolveTypeTags(state, field->type);
          addLocalDecl(&funcState, field);
        }
        semaStmt(&funcState, decl->body);
      }
    case DeclKind::VAR:
      addLocalDecl(state, decl);
      if (decl->init != null) {
        semaExpr(state, decl->init);

        if (decl->type == null) {
          decl->type = decl->init->type;
        } else if (decl->init = doConvert(state, decl->init, decl->type),
            decl->init == null) {
          failSemaDecl(decl, ": Decl init type doesn't match");
        }

        sizeArrayTypes(decl->type, decl->init->type);
      }
    case DeclKind::IMPORT:
      if (state->parent != null) {
        failSemaDecl(decl, "Import not allowed in local scope");
      }

      // Check if we already import this one
      for (let cur = state->imports; cur != null; cur = cur->next) {
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
      if (fileDecls == null) {
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
  let state = SemaState {
    parent = parent,
    result = parent->result,
  };
  return state;
}

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
      &switchType->kind as TypeKind::Enum* != null
      || &switchType->kind as TypeKind::Union* != null;

  if (&switchType->kind as TypeKind::Int* == null && !exhaustive) {
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
      if (stmt->expr == null && &state->result->kind as TypeKind::Void* == null) {
        failSemaStmt(stmt, "Return type should be void");
      }
      if (stmt->expr != null) {
        semaExpr(state, stmt->expr);
        let conv = doConvert(state, stmt->expr, state->result);
        if (conv == null) {
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
      semaExpr(state, stmt->expr);

      // Add != null for let expressions.
      if (stmt->expr->kind == ExprKind::LET) {
        let ptrType = &stmt->expr->type->kind as TypeKind::Pointer*;
        if (ptrType != null) {
          stmt->expr = makeNullCmp(stmt->expr);
        }
      }
      checkBool(stmt->expr);

      semaStmt(state, stmt->init);
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

func addTaggedType(state: SemaState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::STRUCT, DeclKind::ENUM:
      if (findType(state->types, *getTypeTag(decl->type)) != null) {
        failSemaDecl(decl, ": Type redef");
      }

      // Add the struct to the types.
      let type = newDeclList(decl);
      type->next = state->types;
      state->types = type;
    case DeclKind::UNION:
      if (findType(state->types, *getTypeTag(decl->type)) != null) {
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
  for (let cur = decl; cur != null; cur = cur->next) {
    addTaggedType(state, cur);
  }

  for (let cur = decl; cur != null; cur = cur->next) {
    semaDecl(state, cur);
  }

  // Add extra decls
  if (state->extraDecls != null) {
    let last = state->extraDecls;
    while (last->next != null) {
      last = last->next;
    }
    last->next = decl;
    decl = state->extraDecls;
  }

  return decl;
}

func getNullDecl(name: i8*) -> DeclAST* {
  let nullTok = Token {};
  nullTok.kind = TokenKind::IDENTIFIER;
  nullTok.data = name;
  nullTok.end = name + strlen(name);

  // Add null as a nullptr
  let nullDecl = newDecl(DeclKind::ENUM_FIELD);
  nullDecl->name = nullTok;
  nullDecl->enumValue = 0;
  nullDecl->type = newType(TypeKind::Pointer {
    pointee = newType(TypeKind::Void {}),
  });

  return nullDecl;
}

func initSemaState() -> SemaState {
  let semaState = SemaState {};

  let nullDecl = getNullDecl("null");
  semaState.locals = newDeclList(nullDecl);

  return semaState;
}
