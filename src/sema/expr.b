import state;
import type;
import utils;


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
      if (let fromPtr = from->kind as TypeKind::Pointer*) {
        // TODO: Remove and use 'as' once the ': type' syntax is implemented.
        //
        // void * can be converted from and to any other pointer..
        if (fromPtr->pointee->kind as TypeKind::Void* != null
            || toPtr.pointee->kind as TypeKind::Void* != null) {
          return expr;
        }

        // Pointer to arrays can be convert to pointers to the first element.
        // This is a no-op for code gen?
        if (let fromArray = fromPtr->pointee->kind as TypeKind::Array*) {
          if (typeEq(fromArray->element, toPtr.pointee)) {
            return expr;
          }
        }
      }

    // A union member can be converted to the union type by inserting the kind.
    case TypeKind::Union as toUnion:
      if (let fromStruct = from->kind as TypeKind::Struct*) {
        let unionDecl = lookupType(state, toUnion.tag);
        if (unionDecl == null) {
          failSemaExpr(expr, "Can't find union decl to cast to");
        }

        let idx = 0;
        let structDecl =
            findTypeIdx(unionDecl->subTypes, fromStruct->tag, &idx);
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
      }

    default:
      break;
  }

  return null;
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
  resolveTypeTags(state, castExpr->type, castExpr->location);
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
      if (let toInt = to->kind as TypeKind::Int*) {
        return semaIntCast(castExpr, &fromInt, toInt);
      }

      // enums can be casted from integers
      if (to->kind as TypeKind::Enum* != null) {
        let enumInt = TypeKind::Int {
          size = 32,
          isSigned = true,
        };
        return semaIntCast(castExpr, &fromInt, &enumInt);
      }

    case TypeKind::Enum:
      // enums can be casted to integers
      if (let toInt = to->kind as TypeKind::Int*) {
        let enumInt = TypeKind::Int {
          size = 32,
          isSigned = true,
        };
        return semaIntCast(castExpr, &enumInt, toInt);
      }

    case TypeKind::Union as fromUnion:
      if (let toPtr = to->kind as TypeKind::Pointer*) {
        if (let toStruct = toPtr->pointee->kind as TypeKind::Struct*) {
          let unionDecl = lookupType(state, fromUnion.tag);
          if (unionDecl == null) {
            failSemaExpr(expr, "Can't find union decl to cast from");
          }

          let idx = 0;
          let structDecl =
              findTypeIdx(unionDecl->subTypes, toStruct->tag, &idx);
          if (structDecl == null) {
            failSemaExpr(expr, "No way to convert union to unrelated struct");
          }

          // Insert a deref expr.
          let deref = newExpr(ExprKind::UNARY);
          deref->op.kind = TokenKind::AND;
          deref->rhs = castExpr->lhs;
          deref->type = newType(TypeKind::Pointer {
            pointee = deref->rhs->type,
          });
          castExpr->lhs = deref;

          castExpr->value = idx;
          castExpr->castKind = CastKind::UnionStructPtr;
          return 1;
        }
      }

    case TypeKind::Pointer as fromPtr:
      if (let toPtr = to->kind as TypeKind::Pointer*) {
        // void * can be casted from and to any other pointer..
        if (fromPtr.pointee->kind as TypeKind::Void* != null
            || toPtr->pointee->kind as TypeKind::Void* != null) {
          castExpr->castKind = CastKind::Noop;
          return 1;
        }

        // Pointer to arrays can be casted to pointers to the first element.
        // This is a no-op for code gen.
        if (let fromArray = fromPtr.pointee->kind as TypeKind::Array*) {
          if (typeEq(fromArray->element, toPtr->pointee)) {
            castExpr->castKind = CastKind::Noop;
            return 1;
          }
        }

        // Pointers to unions can be converted to pointers to structs.
        let fromUnion = fromPtr.pointee->kind as TypeKind::Union*;
        let toStruct = toPtr->pointee->kind as TypeKind::Struct*;
        if (fromUnion != null && toStruct != null) {
          let unionDecl = lookupType(state, fromUnion->tag);
          if (unionDecl == null) {
            failSemaExpr(expr, "Can't find union decl to cast from");
          }

          let idx = 0;
          let structDecl =
              findTypeIdx(unionDecl->subTypes, toStruct->tag, &idx);
          if (structDecl == null) {
            failSemaExpr(expr, "No way to convert union to unrelated struct");
          }

          castExpr->value = idx;
          castExpr->castKind = CastKind::UnionStructPtr;
          return 1;
        }
      }

    case TypeKind::Struct as fromStruct:
      if (let toUnion = to->kind as TypeKind::Union*) {
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
      }

    default:
      break;
  }

  return 0;
}


func checkBool(expr: ExprAST*) {
  if (expr->type->kind as TypeKind::Bool* == null) {
    failSemaExpr(expr, ": Expected bool!");
  }
}

func getStringLength(tok: Token) -> i32 {
  let len = 0;

  for (let c = tok.data; c < tok.end; c++) {
    if (*c == '\\') {
      c++;
      len++;
    } else {
      len++;
    }
  }

  return len + 1;  // null terminator
}

func semaString(state: SemaState*, expr: ExprAST*) {
  expr->type = newType(TypeKind::Array {
    element = getCharType(),
    size = getStringLength(expr->identifier),
  });

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

func semaBinExpr(state: SemaState*, expr: ExprAST*) {
  semaExpr(state, expr->lhs);
  semaExpr(state, expr->rhs);

  let lhsTypePtr = expr->lhs->type->kind as TypeKind::Pointer*;
  let lhsTypeInt = expr->lhs->type->kind as TypeKind::Int*;

  let rhsTypePtr = expr->rhs->type->kind as TypeKind::Pointer*;
  let rhsTypeInt = expr->rhs->type->kind as TypeKind::Int*;

  // Handle special cases
  switch (expr->op.kind) {
    case TokenKind::COMMA:
      expr->type = expr->rhs->type;
      return;

    // comparision results in i32.
    case TokenKind::LESS, TokenKind::GREATER, TokenKind::LE_OP,
         TokenKind::GE_OP, TokenKind::EQ_OP, TokenKind::NE_OP:
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
      expr->type = getBool();
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

    case TokenKind::AND_OP, TokenKind::OR_OP:
      checkBool(expr->lhs);
      checkBool(expr->rhs);
      expr->type = expr->lhs->type;
      return;

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
        let ptrType = expr->lhs->type->kind as TypeKind::Pointer*;
        let structType = ptrType == null
             ? null as TypeKind::Struct*
             : ptrType->pointee->kind as TypeKind::Struct*;
        if (structType == null) {
          failSemaExpr(expr, ": Expected pointer to struct type for -> expr");
        }
        structDecl = lookupStruct(state, structType);
      } else if (expr->op.kind == TokenKind::DOT) {
        let structType = expr->lhs->type->kind as TypeKind::Struct*;
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
      let funType = expr->lhs->type->kind as TypeKind::Func*;
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
      if (expr->type == null) {
        failSemaExpr(expr, "Expected int type to be set during parsing.");
      }
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

      let array = expr->lhs->type->kind as TypeKind::Array*;
      if (array == null) {
        failSemaExpr(expr, " Index only works on arrays, or pointers to them.");
      }

      semaExpr(state, expr->rhs);
      if (expr->rhs->type->kind as TypeKind::Int* == null) {
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
        let ptrType = expr->rhs->type->kind as TypeKind::Pointer*;
        if (ptrType == null) {
          failSemaExpr(expr, "Expected pointer type for *");
        }
        expr->type = ptrType->pointee;
      }

      // TODO: correct?
      if (expr->op.kind == TokenKind::BANG) {
        expr->type = getBool();
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
      resolveTypeTags(state, expr->decl->type, expr->location);
      semaVarDecl(state, expr->decl);
      expr->type = expr->decl->type;
  }
}

func semaVarDecl(state: SemaState*, decl: DeclAST*) {
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
}
