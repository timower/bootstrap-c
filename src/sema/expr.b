import state;
import type;
import utils;
import eval;
import generics;

const max_sema_depth = 200;

func doConvertBase(state: SemaState*, expr: ExprAST*, to: Type*, isConstStr: bool) -> ExprAST* {
  let from = expr->type;

  if (typeEq(from, to)) {
    return expr;
  }

  switch (to->kind) {
    // Allow integer expression casting
    case TypeKind::Int as i:
      if (let intExpr = expr->kind as ExprKind::Int*) {
        let res = newExpr(state->astAlloc, ExprKind::Int {
          value = intExpr->value,
          token = intExpr->token,
        });
        res->location = expr->location;
        res->type = to;
        return res;
      }

      if (let fromInt = from->kind as TypeKind::Int*) {
        if (fromInt->isSigned != i.isSigned) {
          break;
        }
        if (fromInt->size > i.size) {
          break;
        }

        let res = newExpr(state->astAlloc, ExprKind::Cast {
          castKind = i.isSigned ? CastKind::Sext : CastKind::Zext,
          expr = expr,
        });
        res->type = to;
        res->location = expr->location;
        return res;
      }

    case TypeKind::Slice as toSlice:
      let fromArray = getPointerToArray(from);
      if (fromArray == null) {
        break;
      }

      if (!typeEq(fromArray->element, toSlice.element)) {
        break;
      }

      let sizeExpr = newExpr(state->astAlloc, ExprKind::Int {
        value = fromArray->size,
      });
      sizeExpr->type = getIPtr(state->astAlloc, &state->target);
      let res = newExpr(state->astAlloc, ExprKind::SliceIndex {
        slice = expr,
        end = sizeExpr,
      });
      res->type = to;
      res->location = expr->location;
      return res;

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
        if (isConstStr) {
          let fromArray = fromPtr->pointee->kind as TypeKind::Array*;
          if (fromArray == null) {
            unreachable("Expected array for string expressions");
          }
          if (typeEq(fromArray->element, toPtr.pointee)) {
            return expr;
          }
        }
      }

    // A union member can be converted to the union type by inserting the kind.
    case TypeKind::Union as toUnion:
      if (let fromStruct = from->kind as TypeKind::Struct*) {
        let castExpr = newExpr(state->astAlloc, ExprKind::Cast {
          expr = expr,
          castKind = CastKind::StructUnion,
        });
        castExpr->location = expr->location;
        castExpr->type = to;
        semaCast(state, castExpr);
        return castExpr;
      }

    default:
      break;
  }

  return null;
}

func doConvert(state: SemaState*, expr: ExprAST*, to: Type*) -> ExprAST* {
  return doConvertBase(state, expr, to, false);
}

func semaIntCast(
    castKind: CastKind*,
    fromInt: TypeKind::Int*,
    toInt: TypeKind::Int*
) -> bool {
  // Sign change, no-op for now.
  if (fromInt->size == toInt->size) {
    *castKind = CastKind::Noop;
    return true;
  }

  // Same signedness but different type
  if (fromInt->size > toInt->size) {
    *castKind = CastKind::Trunc;
    return true;
  }

  // Casting from a signed integer, so sign extend.
  if (fromInt->isSigned) {
    *castKind = CastKind::Sext;
    return true;
  }

  // Otherwise zero extend.
  *castKind = CastKind::Zext;
  return true;
}

func semaCast(state: SemaState*, castExpr: ExprAST*) -> bool {
  resolveTypeTags(state, castExpr->type);
  if (castExpr->type == null) {
    unreachable("Cast without type?");
  }

  let cast = castExpr->kind as ExprKind::Cast*;
  let to = castExpr->type;
  let expr = cast->expr;
  let from = expr->type;

  if (typeEq(from, to)) {
    cast->castKind = CastKind::Noop;
    return true;
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
          if (toStruct->parent == null || !typeEq(from, toStruct->parent)) {
            errorSema(
                state,
                expr->location,
                "Cannot cast union to pointer of variant from different union");
            return false;
          }

          let unionDecl = lookupType(state, fromUnion.tag);
          if (unionDecl == null) {
            // resolving type tags should've faied.
            unreachable("Can't find union decl to cast from");
          }

          let idx = 0;
          let structDecl = findSubType(
              state,
              &unionDecl->kind as DeclKind::Union*,
              toStruct->tag,
              &idx);
          if (structDecl == null) {
            // The typeEq above ensures this is unreachable.
            unreachable("No way to convert union to unrelated struct");
          }

          // Insert a deref expr.
          let deref = newExpr(state->astAlloc, ExprKind::Unary {
            op = Token {
              kind = TokenKind::AND,
            },
            postfix = null,
            prefix = cast->expr,
          });
          deref->type = newType(state->astAlloc, TypeKind::Pointer {
            pointee = cast->expr->type,
          });
          cast->expr = deref;

          cast->fieldIndex = idx;
          cast->castKind = CastKind::UnionStructPtr;
          return true;
        }
      }

    case TypeKind::Pointer as fromPtr:
      if (let toPtr = to->kind as TypeKind::Pointer*) {
        // void * can be casted from and to any other pointer..
        if (fromPtr.pointee->kind as TypeKind::Void* != null
            || toPtr->pointee->kind as TypeKind::Void* != null) {
          cast->castKind = CastKind::Noop;
          return true;
        }

        // Pointer to arrays can be casted to pointers to the first element.
        // This is a no-op for code gen.
        // TODO: fix
        // if (let fromArray = fromPtr.pointee->kind as TypeKind::Array*) {
        //   if (typeEq(fromArray->element, toPtr->pointee)) {
        //     cast->castKind = CastKind::Noop;
        //     return 1;
        //   }
        // }
        // Pointers to unions can be converted to pointers to structs.
        let fromUnion = fromPtr.pointee->kind as TypeKind::Union*;
        let toStruct = toPtr->pointee->kind as TypeKind::Struct*;
        if (fromUnion != null && toStruct != null) {
          if (toStruct->parent == null || !typeEq(toStruct->parent, fromPtr.pointee)) {
            errorSema(
                state,
                expr->location,
                "Cannot cast union pointer to pointer of variant from different union");
            return false;
          }

          let unionDecl = lookupType(state, fromUnion->tag);

          if (unionDecl == null) {
            // The typeEq will fail before this.
            unreachable("Can't find union decl to cast from");
          }

          let idx = 0;
          let unionDeclKind = &unionDecl->kind as DeclKind::Union*;
          let structDecl = findSubType(state, unionDeclKind, toStruct->tag, &idx);
          if (structDecl == null) {
            // The typeEq will fail before this.
            unreachable("No way to convert union to unrelated struct");
          }

          cast->fieldIndex = idx;
          cast->castKind = CastKind::UnionStructPtr;
          return true;
        }
      }

    case TypeKind::Struct as fromStruct:
      if (let toUnion = to->kind as TypeKind::Union*) {
        if (fromStruct.parent == null || !typeEq(fromStruct.parent, to)) {
          errorSema(state, expr->location, "Can't cast struct to unrelated union");
          return false;
        }

        let unionDecl = lookupType(state, toUnion->tag);
        if (unionDecl == null) {
          unreachable("Can't find union decl to cast to");
        }

        let idx = 0;
        let structDecl = findSubType(
            state,
            &unionDecl->kind as DeclKind::Union*,
            fromStruct.tag,
            &idx);
        if (structDecl == null) {
          unreachable("No way to convert struct to unrelated union");
        }

        cast->fieldIndex = idx;
        cast->castKind = CastKind::StructUnion;
        return true;
      }

    default:
      break;
  }

  return false;
}


func checkBool(state: SemaState*, expr: ExprAST*) {
  if (expr->type->kind as TypeKind::Bool* == null) {
    failSemaExpr(state, expr, ": Expected bool!");
  }
}

func getStringLength(tok: Token) -> i32 {
  let len = 0;

  let end = tok.data.len;
  for (let i = 0; i < end; i++) {
    if (tok.data[i] == '\\') {
      i++;
      len++;
    } else {
      len++;
    }
  }

  return len + 1;  // null terminator
}


// Does the AST transform of a string expression.
//
// A string constant like "foo" is turned into a global static array:
//  let str.0: i8[3] = ['f', 'o', 'o', 0];
// The expression itself is then replaced with `&str.0`
//
// Note: the array init length is one more than the type due to the 0 terminator.
func semaString(state: SemaState*, expr: ExprAST*) {
  let strExpr = expr->kind as ExprKind::Str*;
  let strLen = getStringLength(strExpr->identifier);
  let init = newExpr(state->astAlloc, ExprKind::Str {
    identifier = strExpr->identifier,
  });

  init->type = newType(state->astAlloc, TypeKind::Array {
    element = getCharType(state->astAlloc),
    size = strLen,
  });

  // Add a global variable for the string.
  let root = getRoot(state);
  let decl = newDecl(state->astAlloc, DeclKind::Var {
    init = init,
  });
  decl->type = newType(state->astAlloc, TypeKind::Array {
    element = getCharType(state->astAlloc),
    size = strLen - 1,
  });

  let name = newInternalToken(state->astAlloc, 32);
  let len = sprintf(&name.data[0], "str.%d", root->strCount++);
  name.data = name.data[:len];

  decl->name = name;
  decl->next = root->extraDecls;
  root->extraDecls = decl;

  let ptrType = newType(state->astAlloc, TypeKind::Pointer {
    pointee = decl->type,
  });
  expr->type = ptrType;

  // transmute expr into a address of expr.
  let varRef = newExpr(state->astAlloc, ExprKind::Variable {
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

func convertTypes(
    state: SemaState*,
    expr: ExprAST*,
    binExpr: ExprKind::Binary*
) {
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

func semaBinExpr(state: SemaState*, expr: ExprAST*) {
  let binExpr = expr->kind as ExprKind::Binary*;
  semaExpr(state, binExpr->lhs);
  semaExpr(state, binExpr->rhs);

  let lhsTypePtr = binExpr->lhs->type->kind as TypeKind::Pointer*;
  let lhsTypeInt = binExpr->lhs->type->kind as TypeKind::Int*;
  let lhsTypeEnum = binExpr->lhs->type->kind as TypeKind::Enum*;
  let lhsTypeBool = binExpr->lhs->type->kind as TypeKind::Bool*;

  // let rhsTypePtr = binExpr->rhs->type->kind as TypeKind::Pointer*;
  // let rhsTypeInt = binExpr->rhs->type->kind as TypeKind::Int*;
  // Handle special cases
  switch (binExpr->op.kind) {
    case TokenKind::COMMA:
      expr->type = binExpr->rhs->type;
      return;

    case TokenKind::EQ:
      break;

    // comparision results in i32.
    case TokenKind::LESS, TokenKind::GREATER, TokenKind::LE_OP,
         TokenKind::GE_OP, TokenKind::EQ_OP, TokenKind::NE_OP:
      if (lhsTypeInt == null && lhsTypePtr == null
          && lhsTypeEnum == null && lhsTypeBool == null) {
        failSemaExpr(state, expr, "Unsupported type for compare");
      }

      convertTypes(state, expr, binExpr);

      expr->type = getBool(state->astAlloc);
      return;

    default:
      if (lhsTypeInt == null) {
        failSemaExpr(state, expr, "Only integers supported");
      }

    case TokenKind::AND_OP, TokenKind::OR_OP:
      checkBool(state, binExpr->lhs);
      checkBool(state, binExpr->rhs);
      expr->type = binExpr->lhs->type;
      return;
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

  convertTypes(state, expr, binExpr);

  expr->type = binExpr->lhs->type;
}

func semaExpr(state: SemaState*, expr: ExprAST*) {
  if (state->depth++ > max_sema_depth) {
    failSemaExpr(state, expr, "Too deep!");
  }

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
            failSemaExpr(state, expr, " Cannot find enum value");
          }

        default:
          failSemaExpr(state, expr, "Expected enum type for scope expr");
      }

      expr->type = decl->type;

    case ExprKind::Member as memberExpr:
      semaExpr(state, memberExpr.object);

      let objectType: Type* = null;
      if (memberExpr.op.kind == TokenKind::PTR_OP) {
        let ptrType = memberExpr.object->type->kind as TypeKind::Pointer*;
        if (ptrType == null) {
          failSemaExpr(state, expr, "Expected pointer for ->");
        }
        objectType = ptrType->pointee;
      } else if (memberExpr.op.kind == TokenKind::DOT) {
        objectType = memberExpr.object->type;
      } else {
        // The parser doesn't allow this.
        unreachable("Unknown member op");
      }

      switch (objectType->kind) {
        case TypeKind::Array as a:
          if (!tokCmpStr(memberExpr.identifier, "len")) {
            failSemaExpr(state, expr, " Only 'len' member supported");
          }

          expr->kind = ExprKind::Int {
            token = memberExpr.identifier,
            value = a.size,
          };
          expr->type = getIPtr(state->astAlloc, &state->target);

        case TypeKind::Slice:
          if (!tokCmpStr(memberExpr.identifier, "len")) {
            failSemaExpr(state, expr, " Only 'len' member supported");
          }

          // (ptr, len) so index 1
          memberExpr.fieldIndex = 1;
          expr->type = getIPtr(state->astAlloc, &state->target);

        case TypeKind::Struct as structType:
          let structDecl = lookupStruct(state, &structType);
          if (structDecl == null) {
            failSemaExpr(state, expr, "Unknown type for member expression");
          }

          let fieldDecl = findField(state, structDecl, memberExpr.identifier, &memberExpr.fieldIndex);
          if (fieldDecl == null) {
            failSemaExpr(state, expr, " Cannot find field");
          }

          expr->type = fieldDecl->type;

        default:
          failSemaExpr(state, expr, ": Expected struct type for member access");
      }

    case ExprKind::GenericInstantiation as genericInst:
      resolveTypeTags(state, genericInst.typeArgs);

      let local = lookupLocal(state, genericInst.function);
      if (local == null) {
        failSemaExpr(state, expr, "Couldn't find variable in scope");
      }
      if (local->kind as DeclKind::Func* == null) {
        failSemaExpr(state, expr, "Expected function declaration");
      }

      let fnType = local->type->kind as TypeKind::Func*;
      if (fnType == null) {
        unreachable("Function decl with non-function type");
      }
      if (fnType->typeArgs == null) {
        failSemaExpr(state, expr, "Expected generic function type");
      }

      let mapping = getTypeMap(state->astAlloc, fnType, genericInst.typeArgs);
      if (mapping == null) {
        failSemaExpr(state, expr, "Failed to instantiate, incorrect number of type args");
      }

      expr->type = substituteTypeWithMapping(
          state->astAlloc,
          local->type,
          mapping);
      if (expr->type == null) {
        unreachable("Failed to instantiate");
      }
      resolveTypeTags(state, expr->type);
      genericInst.instance = addGenericInst(state, local, mapping);

    case ExprKind::Call as callExpr:
      semaExpr(state, callExpr.function);

      let funType = getFunctionType(&callExpr);
      if (funType == null) {
        failSemaExpr(state, expr, "Must call function or function pointer type");
      }

      let curArgTy = funType->args;
      let cur = callExpr.args;
      let last: ExprAST** = &callExpr.args;
      for (; cur != null; cur = cur->next) {
        // Cache the string expression kind before sema transforms it.
        let isStringExpr = cur->kind as ExprKind::Str* != null;
        semaExpr(state, cur);

        if (curArgTy != null) {
          let conv = doConvertBase(state, cur, curArgTy, isStringExpr);
          if (conv == null) {
            printType(curArgTy);
            failSemaExpr(state, expr, " Arg type mismatch");
          }

          if (conv != cur) {
            // Chain in 'conv' to replace 'cur'
            conv->next = cur->next;
            cur->next = null;
            cur = conv;

            *last = conv;
          }
        } else if (!funType->isVarargs) {
          break;
        }

        last = &cur->next;

        if (curArgTy != null) {
          curArgTy = curArgTy->next;
        }
      }

      if (((curArgTy == null) != (cur == null))) {
        errorSema(state, expr->location, "Function call arg length mismatch");
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
        // sub->type = doDecay(sub->type);
        if (elementType == null) {
          elementType = sub->type;
        } else if (!typeEq(elementType, sub->type)) {
          failSemaExpr(state, expr, "Init must have consistent type");
        }

        size++;
      }

      expr->type = newType(state->astAlloc, TypeKind::Array {
        size = size,
        element = elementType,
      });

    case ExprKind::Str:
      semaString(state, expr);

    case ExprKind::Variable as varExpr:
      let local = lookupLocal(state, varExpr.identifier);
      if (local == null) {
        failSemaExpr(state, expr, "Couldn't find variable in scope");
      }

      if (let type = local->type->kind as TypeKind::Func*) {
        if (type->typeArgs != null) {
          failSemaExpr(state, expr, "Uninstantiated generic expression");
        }
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
        unreachable("Expected int type to be set during parsing.");
      }

    case ExprKind::Binary:
      semaBinExpr(state, expr);

    case ExprKind::Index as indexExpr:
      semaExpr(state, indexExpr.array);

      let elementType: Type* = null;
      switch (indexExpr.array->type->kind) {
        case TypeKind::Array as a:
          elementType = a.element;
        case TypeKind::Slice as s:
          elementType = s.element;
        default:
          failSemaExpr(state, expr, " Index only works on arrays, or slices.");
      }

      semaExpr(state, indexExpr.index);
      if (indexExpr.index->type->kind as TypeKind::Int* == null) {
        failSemaExpr(state, expr, "Can't index with non integer");
      }
      expr->type = elementType;

    case ExprKind::SliceIndex as slice:
      semaExpr(state, slice.slice);
      let elementType: Type* = null;
      switch (slice.slice->type->kind) {
        case TypeKind::Array as a:
          elementType = a.element;
        case TypeKind::Slice as s:
          elementType = s.element;
        case TypeKind::Pointer as p:
          let ptrToArray = getPointerToArray(slice.slice->type);
          if (ptrToArray != null) {
            if (slice.end == null) {
              slice.end = newExpr(state->astAlloc, ExprKind::Int {
                value = ptrToArray->size,
              });
              slice.end->type = getIPtr(state->astAlloc, &state->target);
            }
            elementType = ptrToArray->element;
          } else {
            // TODO: remove
            if (slice.end == null) {
              failSemaExpr(state, expr, " Pointer to slice requires end");
            }
            elementType = p.pointee;
          }
        default:
          failSemaExpr(state, expr, " Expected slice or array");
      }

      if (slice.start != null) {
        semaExpr(state, slice.start);
        if (slice.start->type->kind as TypeKind::Int* == null) {
          failSemaExpr(state, expr, "Start expression must be integer");
        }
        slice.start = newExpr(state->astAlloc, ExprKind::Cast {
          expr = slice.start,
        });
        slice.start->type = getIPtr(state->astAlloc, &state->target);
        semaCast(state, slice.start);
      }

      if (slice.end != null) {
        semaExpr(state, slice.end);
        if (slice.end->type->kind as TypeKind::Int* == null) {
          failSemaExpr(state, expr, "End expression must be integer");
        }
        slice.end = newExpr(state->astAlloc, ExprKind::Cast {
          expr = slice.end,
        });
        slice.end->type = getIPtr(state->astAlloc, &state->target);
        semaCast(state, slice.end);
      }

      expr->type = newType(state->astAlloc, TypeKind::Slice {
        element = elementType,
      });

    case ExprKind::Unary as unaryExpr:
      if (unaryExpr.op.kind == TokenKind::AND) {
        semaExpr(state, unaryExpr.prefix);
        expr->type = newType(state->astAlloc, TypeKind::Pointer {
          pointee = unaryExpr.prefix->type,
        });
        break;
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
      } else if (expr->type->kind as TypeKind::Pointer* != null) {
        failSemaExpr(state, expr, "Unary on pointer");
      }

      if (unaryExpr.op.kind == TokenKind::BANG) {
        checkBool(state, expr);
      }

    case ExprKind::Sizeof as sizeofExpr:
      resolveTypeTags(state, sizeofExpr.typeArg);
      sizeofExpr.value = getSize(state, sizeofExpr.typeArg, null);

      expr->kind = ExprKind::Int {
        value = sizeofExpr.value,
      };
      expr->type = getUPtr(state->astAlloc, &state->target);

    case ExprKind::Cast as castExpr:
      semaExpr(state, castExpr.expr);
      if (!semaCast(state, expr)) {
        failSemaExpr(state, expr, " Can't cast");
      }

    case ExprKind::Paren as parenExpr:
      semaExpr(state, parenExpr.expr);
      expr->type = parenExpr.expr->type;

    case ExprKind::Let as letExpr:
      switch (letExpr.decl->kind) {
        case DeclKind::Var as varKind:
          if (varKind.init == null) {
            failSemaExpr(state, expr, "Let expression must have an init");
          }
        case DeclKind::Const as constKind:
          if (constKind.init == null) {
            failSemaExpr(state, expr, "Const expression must have an init");
          }
        default:
          // The parser doesn't allow this.
          unreachable("Only let expressions allowed");
      }

      resolveTypeTags(state, letExpr.decl->type);
      semaVarDecl(state, letExpr.decl);
      expr->type = letExpr.decl->type;
  }

  if (expr->type == null) {
    unreachable("Type should always be set");
  }

  // TODO: defer
  state->depth--;
}

func semaVarDecl(state: SemaState*, decl: DeclAST*) {
  let init: ExprAST* = null;
  switch (decl->kind) {
    case DeclKind::Var as varKind:
      init = varKind.init;
    case DeclKind::Const as constKind:
      init = constKind.init;
    default:
      unreachable("Expected const or var decl kind");
  }

  if (init != null) {
    semaExpr(state, init);

    if (decl->type == null) {
      decl->type = init->type;
    } else {
      sizeArrayTypes(state, decl->type, init->type);
      if (init = doConvert(state, init, decl->type), init == null) {
        failSemaDecl(state, decl, ": Decl init type doesn't match");
      }
    }

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
          default:
            // This is impossibe as evalConstant only returns ints or scopes.
            unreachable("Const decl must have an int init");
        }

      default:
        break;
    }
  } else if (&decl->kind as DeclKind::Const* != null) {
    failSemaDecl(state, decl, "Const decl must have an init");
  } else if (isUnsized(decl->type)) {
    failSemaDecl(state, decl, "Decl with unsized type must have init");
  }

  addLocalDecl(state, decl);
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
    case TypeKind::Slice as a:
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
