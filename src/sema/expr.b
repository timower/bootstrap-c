import state;
import type;
import utils;
import eval;
import generics;


func doConvert(state: SemaState*, expr: ExprAST*, to: Type*) -> ExprAST* {
  let from = expr->type;

  if (typeEq(from, to)) {
    return expr;
  }

  switch (to->kind) {
    // Allow integer expression casting
    case TypeKind::Int:
      if (let intExpr = expr->kind as ExprKind::Int*) {
        let res = newExpr(ExprKind::Int {
          value = intExpr->value,
          token = intExpr->token,
        });
        res->location = expr->location;
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
          failSemaExpr(state, expr, "Can't find union decl to cast to");
        }

        let idx = 0;
        let unionDeclKind = &unionDecl->kind as DeclKind::Union*;
        let structDecl = findSubType(state, unionDeclKind, fromStruct->tag, &idx);
        if (structDecl == null) {
          failSemaExpr(state, expr, "No way to convert struct to unrelated union");
        }

        let castExpr = newExpr(ExprKind::Cast {
          expr = expr,
          castKind = CastKind::StructUnion,
          fieldIndex = idx,
        });
        castExpr->location = expr->location;
        castExpr->type = to;
        return castExpr;
      }

    default:
      break;
  }

  return null;
}


func semaIntCast(
    castKind: CastKind*,
    fromInt: TypeKind::Int*,
    toInt: TypeKind::Int*
) -> i32 {
  // Sign change, no-op for now.
  if (fromInt->size == toInt->size) {
    *castKind = CastKind::Noop;
    return 1;
  }

  // Same signedness but different type
  if (fromInt->size > toInt->size) {
    *castKind = CastKind::Trunc;
    return 1;
  }

  if (fromInt->isSigned && toInt->isSigned) {
    *castKind = CastKind::Sext;
    return 1;
  }

  if (!fromInt->isSigned && !toInt->isSigned) {
    *castKind = CastKind::Zext;
    return 1;
  }

  return 0;
}

func semaCast(state: SemaState*, castExpr: ExprAST*) -> i32 {
  resolveTypeTags(state, castExpr->type);
  if (castExpr->type == null) {
    failSemaExpr(state, castExpr, "Cast without type?");
  }

  let cast = castExpr->kind as ExprKind::Cast*;
  let to = castExpr->type;
  let expr = cast->expr;
  let from = expr->type;

  if (typeEq(from, to)) {
    cast->castKind = CastKind::Noop;
    return 1;
  }

  // Allow casting int expressions '64 as i32'
  if (let intExpr = expr->kind as ExprKind::Int*) {
    cast->castKind = CastKind::Noop;
    expr->type = to;
    return 1;
  }

  switch (from->kind) {
    case TypeKind::Int as fromInt:
      if (let toInt = to->kind as TypeKind::Int*) {
        return semaIntCast(&cast->castKind, &fromInt, toInt);
      }

      // enums can be casted from integers
      if (to->kind as TypeKind::Enum* != null) {
        let enumInt = TypeKind::Int {
          size = 32,
          isSigned = true,
        };
        return semaIntCast(&cast->castKind, &fromInt, &enumInt);
      }

    case TypeKind::Enum:
      // enums can be casted to integers
      if (let toInt = to->kind as TypeKind::Int*) {
        let enumInt = TypeKind::Int {
          size = 32,
          isSigned = true,
        };
        return semaIntCast(&cast->castKind, &enumInt, toInt);
      }

    case TypeKind::Union as fromUnion:
      if (let toPtr = to->kind as TypeKind::Pointer*) {
        if (let toStruct = toPtr->pointee->kind as TypeKind::Struct*) {
          // Check if target struct belongs to the same union as source
          if (!typeEq(from, toStruct->parent)) {
            failSemaExpr(state, expr, "Cannot cast union to pointer of variant from different union");
          }

          let unionDecl = lookupType(state, fromUnion.tag);
          if (unionDecl == null) {
            failSemaExpr(state, expr, "Can't find union decl to cast from");
          }

          let idx = 0;
          let structDecl = findSubType(state, &unionDecl->kind as DeclKind::Union*, toStruct->tag, &idx);
          if (structDecl == null) {
            failSemaExpr(state, expr, "No way to convert union to unrelated struct");
          }

          // Insert a deref expr.
          let deref = newExpr(ExprKind::Unary {
            op = Token {
              kind = TokenKind::AND,
            },
            postfix = null,
            prefix = cast->expr,
          });
          deref->type = newType(TypeKind::Pointer {
            pointee = cast->expr->type,
          });
          cast->expr = deref;

          cast->fieldIndex = idx;
          cast->castKind = CastKind::UnionStructPtr;
          return 1;
        }
      }

    case TypeKind::Pointer as fromPtr:
      if (let toPtr = to->kind as TypeKind::Pointer*) {
        // void * can be casted from and to any other pointer..
        if (fromPtr.pointee->kind as TypeKind::Void* != null
            || toPtr->pointee->kind as TypeKind::Void* != null) {
          cast->castKind = CastKind::Noop;
          return 1;
        }

        // Pointer to arrays can be casted to pointers to the first element.
        // This is a no-op for code gen.
        if (let fromArray = fromPtr.pointee->kind as TypeKind::Array*) {
          if (typeEq(fromArray->element, toPtr->pointee)) {
            cast->castKind = CastKind::Noop;
            return 1;
          }
        }

        // Pointers to unions can be converted to pointers to structs.
        let fromUnion = fromPtr.pointee->kind as TypeKind::Union*;
        let toStruct = toPtr->pointee->kind as TypeKind::Struct*;
        if (fromUnion != null && toStruct != null) {
          let unionDecl = lookupType(state, fromUnion->tag);
          if (unionDecl == null) {
            failSemaExpr(state, expr, "Can't find union decl to cast from");
          }

          let idx = 0;
          let unionDeclKind = &unionDecl->kind as DeclKind::Union*;
          let structDecl = findSubType(state, unionDeclKind, toStruct->tag, &idx);
          if (structDecl == null) {
            failSemaExpr(state, expr, "No way to convert union to unrelated struct");
          }

          cast->fieldIndex = idx;
          cast->castKind = CastKind::UnionStructPtr;
          return 1;
        }
      }

    case TypeKind::Struct as fromStruct:
      if (let toUnion = to->kind as TypeKind::Union*) {
        let unionDecl = lookupType(state, toUnion->tag);
        if (unionDecl == null) {
          failSemaExpr(state, expr, "Can't find union decl to cast to");
        }

        let idx = 0;
        let structDecl = findSubType(state, &unionDecl->kind as DeclKind::Union*, fromStruct.tag, &idx);
        if (structDecl == null) {
          failSemaExpr(state, expr, "No way to convert struct to unrelated union");
        }

        cast->fieldIndex = idx;
        cast->castKind = CastKind::StructUnion;
        return 1;
      }

    default:
      break;
  }

  return 0;
}


func checkBool(state: SemaState*, expr: ExprAST*) {
  if (expr->type->kind as TypeKind::Bool* == null) {
    failSemaExpr(state, expr, ": Expected bool!");
  }
}

func getStringLength(tok: Token) -> i32 {
  let len = 0;

  let data = tok.location->data;
  let end = data + tok.len;
  for (let c = data; c < end; c++) {
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
  let strExpr = expr->kind as ExprKind::Str*;
  expr->type = newType(TypeKind::Array {
    element = getCharType(),
    size = getStringLength(strExpr->identifier),
  });

  let init = newExpr(ExprKind::Str {
    identifier = strExpr->identifier,
  });
  init->type = expr->type;

  // Add a global variable for the string.
  let root = getRoot(state);
  let decl = newDecl(DeclKind::Var {
    init = init,
  });
  decl->type = expr->type;

  let name = newInternalToken(32);
  name.len = sprintf(name.location->data, "str.%d", root->strCount++);

  decl->name = name;
  decl->next = root->extraDecls;
  root->extraDecls = decl;

  let ptrType = newType(TypeKind::Pointer {
    pointee = decl->type,
  });
  expr->type = ptrType;

  // transmute expr into a address of expr.
  let varRef = newExpr(ExprKind::Variable {
    identifier = decl->name,
  });

  expr->kind = ExprKind::Unary {
    op = Token {
      kind = TokenKind::AND,
    },
    postfix = null,
    prefix = varRef,
  };
}

func semaBinExpr(state: SemaState*, expr: ExprAST*) {
  let binExpr = expr->kind as ExprKind::Binary*;
  semaExpr(state, binExpr->lhs);
  semaExpr(state, binExpr->rhs);

  let lhsTypePtr = binExpr->lhs->type->kind as TypeKind::Pointer*;
  let lhsTypeInt = binExpr->lhs->type->kind as TypeKind::Int*;

  let rhsTypePtr = binExpr->rhs->type->kind as TypeKind::Pointer*;
  let rhsTypeInt = binExpr->rhs->type->kind as TypeKind::Int*;

  // Handle special cases
  switch (binExpr->op.kind) {
    case TokenKind::COMMA:
      expr->type = binExpr->rhs->type;
      return;

    // comparision results in i32.
    case TokenKind::LESS, TokenKind::GREATER, TokenKind::LE_OP,
         TokenKind::GE_OP, TokenKind::EQ_OP, TokenKind::NE_OP:
      if (!typeEq(binExpr->lhs->type, binExpr->rhs->type)) {
        let lhsConv = doConvert(state, binExpr->lhs, binExpr->rhs->type);

        if (lhsConv == null) {
          let rhsConv = doConvert(state, binExpr->rhs, binExpr->lhs->type);
          if (rhsConv == null) {
            failSemaExpr(state, expr, ": Binary op on different types");
          }
          binExpr->rhs = rhsConv;
        } else {
          binExpr->lhs = lhsConv;
        }
      }
      expr->type = getBool();
      return;

    case TokenKind::MINUS:
      if (lhsTypePtr != null && rhsTypePtr != null) {
        if (!typeEq(lhsTypePtr->pointee, getCharType())
            || !typeEq(rhsTypePtr->pointee, lhsTypePtr->pointee)) {
          // TODO: emit (expr) / sizeof(type)
          failSemaExpr(state, expr, "Only char pointer subtract supported");
        }

        expr->type = getIPtr(&state->target);
        return;
      }
      if (lhsTypePtr != null && rhsTypeInt != null) {
        expr->type = binExpr->lhs->type;
        return;
      }
    case TokenKind::PLUS:
      if (lhsTypeInt != null && rhsTypePtr != null) {
        expr->type = binExpr->rhs->type;
        return;
      }
      if (lhsTypePtr != null && rhsTypeInt != null) {
        expr->type = binExpr->lhs->type;
        return;
      }
    case TokenKind::ADD_ASSIGN, TokenKind::SUB_ASSIGN:
      if (lhsTypePtr != null && rhsTypeInt != null) {
        expr->type = binExpr->lhs->type;
        return;
      }

    case TokenKind::AND_OP, TokenKind::OR_OP:
      checkBool(state, binExpr->lhs);
      checkBool(state, binExpr->rhs);
      expr->type = binExpr->lhs->type;
      return;

    default:
      break;
  }

  if (isAssign(binExpr->op)) {
    let conv = doConvert(state, binExpr->rhs, binExpr->lhs->type);
    if (conv == null) {
      failSemaExpr(state, expr, ": Assign doesn't match");
    }
    binExpr->rhs = conv;
    expr->type = binExpr->lhs->type;
    return;
  }

  if (!typeEq(binExpr->lhs->type, binExpr->rhs->type)) {
    let lhsConv = doConvert(state, binExpr->lhs, binExpr->rhs->type);
    if (lhsConv == null) {
      let rhsConv = doConvert(state, binExpr->rhs, binExpr->lhs->type);
      if (rhsConv == null) {
        failSemaExpr(state, expr, ": type mismatch");
      }
      binExpr->rhs = rhsConv;
    } else {
      binExpr->lhs = lhsConv;
    }
  }

  expr->type = binExpr->lhs->type;
}

func semaExpr(state: SemaState*, expr: ExprAST*) {
  switch (expr->kind) {
    case ExprKind::Struct as structExpr:
      let typeDecl: DeclAST* = null;
      if (structExpr.parent.kind != TokenKind::TOK_EOF) {
        let parentDecl = lookupType(state, structExpr.parent);
        if (parentDecl == null) {
          failSemaExpr(state, expr, "Parent type not found");
        }
        let unionDecl = parentDecl->kind as DeclKind::Union*;
        if (unionDecl == null) {
          failSemaExpr(state, expr, "Expected union type");
        }
        typeDecl = findSubType(state, unionDecl, structExpr.identifier, null);
      } else {
        typeDecl = lookupType(state, structExpr.identifier);
      }
      if (typeDecl == null || &typeDecl->kind as DeclKind::Struct* == null) {
        failSemaExpr(state, expr, "Expected struct type for struct init expression");
      }

      // TODO: verify field completeness.
      for (let field = structExpr.fieldIndices; field != null; field = field->next) {
        let fieldDecl = findField(state, typeDecl, field->fieldName, &field->index);
        if (fieldDecl == null) {
          failSemaExpr(state, field->value, " cannot find field");
        }

        semaExpr(state, field->value);
        let conv = doConvert(state, field->value, fieldDecl->type);
        if (conv == null) {
          failSemaExpr(state, field->value, "cannot convert to field type");
        }
        field->value = conv;
      }

      expr->type = typeDecl->type;

    case ExprKind::Scope as scopeExpr:
      let decl = lookupType(state, scopeExpr.parent);
      if (decl == null) {
        failSemaExpr(state, expr, "Unknown type for scope expr");
      }

      switch (decl->kind) {
        case DeclKind::Enum:
          let fieldDecl = findField(state, decl, scopeExpr.identifier, &scopeExpr.enumValue);
          if (fieldDecl == null) {
            failSemaExpr(state, expr, " Cannot find field");
          }

        default:
          failSemaExpr(state, expr, "Expected enum type for scope expr");
      }

      expr->type = decl->type;

    case ExprKind::Member as memberExpr:
      semaExpr(state, memberExpr.object);

      let structDecl = null;
      if (memberExpr.op.kind == TokenKind::PTR_OP) {
        let ptrType = memberExpr.object->type->kind as TypeKind::Pointer*;
        let structType = ptrType == null
             ? null as TypeKind::Struct*
             : ptrType->pointee->kind as TypeKind::Struct*;
        if (structType == null) {
          failSemaExpr(state, expr, ": Expected pointer to struct type for -> expr");
        }
        structDecl = lookupStruct(state, structType);
      } else if (memberExpr.op.kind == TokenKind::DOT) {
        let structType = memberExpr.object->type->kind as TypeKind::Struct*;
        if (structType == null) {
          failSemaExpr(state, expr, "Expected struct type for . expr");
        }
        structDecl = lookupStruct(state, structType);
      } else {
        failSemaExpr(state, expr, "Unknown member op");
      }

      if (structDecl == null) {
        failSemaExpr(state, expr, "Unknown type for member expression");
      }

      let fieldDecl = findField(state, structDecl, memberExpr.identifier, &memberExpr.fieldIndex);
      if (fieldDecl == null) {
        failSemaExpr(state, expr, " Cannot find field");
      }

      expr->type = fieldDecl->type;

    case ExprKind::GenericInstantiation as genericInst:
      resolveTypeTags(state, genericInst.typeArgs);

      let local = lookupLocal(state, genericInst.function);
      if (local == null || local->kind as DeclKind::Func* == null) {
        failSemaExpr(state, expr, "Failed to find function");
      }

      let fnType = local->type->kind as TypeKind::Func*;
      if (fnType == null || fnType->typeArgs == null) {
        failSemaExpr(state, expr, "Expected generic function type");
      }

      let mapping = getTypeMap(fnType, genericInst.typeArgs);
      if (mapping == null) {
        failSemaExpr(state, expr, "Failed to instantiate");
      }

      expr->type = substituteTypeWithMapping(local->type, mapping);
      if (expr->type == null) {
        failSemaExpr(state, expr, "Failed to instantiate");
      }
      resolveTypeTags(state, expr->type);
      genericInst.instance = addGenericInst(state, local, mapping);

    case ExprKind::Call as callExpr:
      semaExpr(state, callExpr.function);

      if (callExpr.function == null) {
        failSemaExpr(state, expr, "Function callee null");
      }
      if (callExpr.function->type == null) {
        failSemaExpr(state, expr, "Function callee type null");
      }

      let funType = getFunctionType(&callExpr);
      if (funType == null) {
        failSemaExpr(state, expr, "Must call function or function pointer type");
      }

      let curArgTy = funType->args;
      let cur = callExpr.args;
      let last: ExprAST** = &callExpr.args;
      for (; cur != null; cur = cur->next) {
        semaExpr(state, cur);

        if (curArgTy != null) {
          let conv = doConvert(state, cur, curArgTy);
          if (conv == null) {
            printType(curArgTy);
            failSemaExpr(state, expr, " Arg type mismatch");
          }

          if (conv != cur) {
            // Chain in 'conv' to replace 'cur'
            conv->next = cur->next;
            cur->next = null;
            cur = conv;
            if (last != null) {
              *last = conv;
            }
          }
        } else if (!funType->isVarargs) {
          break;
        }

        last = &cur->next;

        if (curArgTy != null) {
          curArgTy = curArgTy->next;
        }
      }

      let isValidVararg = funType->isVarargs && curArgTy == null;
      if (!isValidVararg && ((curArgTy == null) != (cur == null))) {
        failSemaExpr(state, expr, "Function call arg length mismatch");
      }
      expr->type = funType->result;

    case ExprKind::Conditional as condExpr:
      semaExpr(state, condExpr.cond);
      checkBool(state, condExpr.cond);
      semaExpr(state, condExpr.trueExpr);
      semaExpr(state, condExpr.falseExpr);

      if (!typeEq(condExpr.trueExpr->type, condExpr.falseExpr->type)) {
        failSemaExpr(state, expr, "?: lhs and rhs should have same type");
      }
      expr->type = condExpr.trueExpr->type;

    case ExprKind::Array as arrayExpr:
      let size = 0;
      let elementType: Type* = null;
      for (let sub = arrayExpr.elements; sub != null; sub = sub->next) {
        semaExpr(state, sub);

        // Decay types in arrays.
        sub->type = doDecay(sub->type);

        if (elementType == null) {
          elementType = sub->type;
        } else if (!typeEq(elementType, sub->type)) {
          failSemaExpr(state, expr, "Init must have consistent type");
        }

        size++;
      }

      expr->type = newType(TypeKind::Array {
        size = size,
        element = elementType,
      });

    case ExprKind::Str:
      semaString(state, expr);

    case ExprKind::Variable as varExpr:
      let local = lookupLocal(state, varExpr.identifier);
      if (local == null || local->type == null) {
        failSemaExpr(state, expr, "Couldn't find variable in scope");
      }

      // enum value, transform this expr to an i32.
      switch (local->kind) {
        case DeclKind::EnumField as enumFieldKind:
          expr->kind = ExprKind::Int {
            value = enumFieldKind.enumValue,
            token = varExpr.identifier,
          };
        case DeclKind::Const as constKind:
          expr->kind = ExprKind::Int {
            value = constKind.enumValue,
            token = varExpr.identifier,
          };
        default:
          break;
      }

      expr->type = local->type;

    case ExprKind::Int:
      if (expr->type == null) {
        failSemaExpr(state, expr, "Expected int type to be set during parsing.");
      }
      return;

    case ExprKind::Binary:
      semaBinExpr(state, expr);

    case ExprKind::Index as indexExpr:
      semaExpr(state, indexExpr.array);

      let ptrToArray = getPointerToArray(indexExpr.array->type);
      if (ptrToArray != null) {
        // auto insert deref to turn expr into an array.
        let derefExpr = newExpr(ExprKind::Unary {
          op = Token {
            kind = TokenKind::STAR,
          },
          prefix = indexExpr.array,
        });
        derefExpr->type = newType(*ptrToArray);

        indexExpr.array = derefExpr;
      }

      let array = indexExpr.array->type->kind as TypeKind::Array*;
      if (array == null) {
        failSemaExpr(state, expr, " Index only works on arrays, or pointers to them.");
      }

      semaExpr(state, indexExpr.index);
      if (indexExpr.index->type->kind as TypeKind::Int* == null) {
        failSemaExpr(state, expr, "Can't index with non integer");
      }
      expr->type = array->element;

    case ExprKind::Unary as unaryExpr:
      if (unaryExpr.op.kind == TokenKind::AND) {
        semaExpr(state, unaryExpr.prefix);
        expr->type = newType(TypeKind::Pointer {
          pointee = unaryExpr.prefix->type,
        });
        return;
      }

      if (unaryExpr.postfix != null) {
        semaExpr(state, unaryExpr.postfix);
        expr->type = unaryExpr.postfix->type;
      } else {
        semaExpr(state, unaryExpr.prefix);
        expr->type = unaryExpr.prefix->type;
      }

      // Handle the specials
      if (unaryExpr.op.kind == TokenKind::STAR) {
        let ptrType = unaryExpr.prefix->type->kind as TypeKind::Pointer*;
        if (ptrType == null) {
          failSemaExpr(state, expr, "Expected pointer type for *");
        }
        expr->type = ptrType->pointee;
      }

      // TODO: correct?
      if (unaryExpr.op.kind == TokenKind::BANG) {
        expr->type = getBool();
      }

    case ExprKind::Sizeof as sizeofExpr:
      resolveTypeTags(state, sizeofExpr.typeArg);
      sizeofExpr.value = getSize(state, sizeofExpr.typeArg);

      expr->kind = ExprKind::Int {
        value = sizeofExpr.value,
      };
      expr->type = getUPtr(&state->target);

    case ExprKind::Cast as castExpr:
      semaExpr(state, castExpr.expr);
      if (!semaCast(state, expr)) {
        failSemaExpr(state, expr, " Can't cast");
      }

    case ExprKind::Paren as parenExpr:
      semaExpr(state, parenExpr.expr);
      expr->type = parenExpr.expr->type;

    case ExprKind::Let as letExpr:
      let init: ExprAST* = null;
      switch (letExpr.decl->kind) {
        case DeclKind::Var as varKind:
          init = varKind.init;
        case DeclKind::Const as constKind:
          init = constKind.init;
        default:
          failSemaExpr(state, expr, "Only let expressions allowed");
      }
      if (init == null) {
        failSemaExpr(state, expr, "Let expression must have an init");
      }
      resolveTypeTags(state, letExpr.decl->type);
      semaVarDecl(state, letExpr.decl);
      expr->type = letExpr.decl->type;
  }
}

func semaVarDecl(state: SemaState*, decl: DeclAST*) {
  addLocalDecl(state, decl);

  let init: ExprAST* = null;
  switch (decl->kind) {
    case DeclKind::Var as varKind:
      init = varKind.init;
    case DeclKind::Const as constKind:
      init = constKind.init;
    default:
      break;
  }

  if (init != null) {
    semaExpr(state, init);

    if (decl->type == null) {
      decl->type = init->type;
    } else if (init = doConvert(state, init, decl->type),
        init == null) {
      failSemaDecl(state, decl, ": Decl init type doesn't match");
    }

    sizeArrayTypes(decl->type, init->type);

    // Update the init field in the union
    switch (decl->kind) {
      case DeclKind::Var as varKind:
        varKind.init = init;

      case DeclKind::Const as constKind:
        // Now we can handle more const expressions thanks to evalConstant
        init = evalConstant(state, init);
        constKind.init = init;

        switch (init->kind) {
          case ExprKind::Int as intExpr:
            constKind.enumValue = intExpr.value;
          case ExprKind::Scope as scopeExpr:
            constKind.enumValue = scopeExpr.enumValue;
          default:
            failSemaDecl(state, decl, "Const decl must have an int init");
        }

      default:
        break;
    }
  } else if (&decl->kind as DeclKind::Const* != null) {
    failSemaDecl(state, decl, "Const decl must have an init");
  }
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
          failSemaType(state, type, "Can't resolve type tags, unknown parent type");
        }

        let tagDecl = findSubType(state, &parentDecl->kind as DeclKind::Union*, tagType.tag, null);
        if (tagDecl == null) {
          failSemaType(state, type, "Can't resolve type tags, unknown sub type");
        }

        let next = type->next;
        *type = *tagDecl->type;
        type->next = next;
      } else {
        let typeDecl = lookupType(state, tagType.tag);
        if (typeDecl == null) {
          failSemaType(state, type, "Can't resolve type tags, unknown type");
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

    case TypeKind::Int as i:
      if (i.isPtr) {
        i.size = getIntSize(&state->target);
      }

    case TypeKind::Typeof as typeofType:
      semaExpr(state, typeofType.expr);

      let next = type->next;
      *type = *typeofType.expr->type;
      type->next = next;

    // TODO: is struct parent needed?
    default:
      break;
  }

  resolveTypeTags(state, type->next);
}
