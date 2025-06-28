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
          failSemaExpr(expr, "Can't find union decl to cast to");
        }

        let idx = 0;
        let unionDeclKind = &unionDecl->kind as DeclKind::Union*;
        let structDecl = findTypeIdx(unionDeclKind->subTypes, fromStruct->tag, &idx);
        if (structDecl == null) {
          failSemaExpr(expr, "No way to convert struct to unrelated union");
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
  resolveTypeTags(state, castExpr->type, castExpr->location);
  if (castExpr->type == null) {
    failSemaExpr(castExpr, "Cast without type?");
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
          let unionDecl = lookupType(state, fromUnion.tag);
          if (unionDecl == null) {
            failSemaExpr(expr, "Can't find union decl to cast from");
          }

          let idx = 0;
          let structDecl =
              findTypeIdx((&unionDecl->kind as DeclKind::Union*)->subTypes, toStruct->tag, &idx);
          if (structDecl == null) {
            failSemaExpr(expr, "No way to convert union to unrelated struct");
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
            failSemaExpr(expr, "Can't find union decl to cast from");
          }

          let idx = 0;
          let unionDeclKind = &unionDecl->kind as DeclKind::Union*;
          let structDecl = findTypeIdx(unionDeclKind->subTypes, toStruct->tag, &idx);
          if (structDecl == null) {
            failSemaExpr(expr, "No way to convert union to unrelated struct");
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
          failSemaExpr(expr, "Can't find union decl to cast to");
        }

        let idx = 0;
        let structDecl = findTypeIdx((&unionDecl->kind as DeclKind::Union*)->subTypes, fromStruct.tag, &idx);
        if (structDecl == null) {
          failSemaExpr(expr, "No way to convert struct to unrelated union");
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

  let name: i8* = malloc(32 as u64);
  let n = sprintf(name, "str.%d", root->strCount++);
  decl->name.kind = TokenKind::IDENTIFIER;
  decl->name.data = name;
  decl->name.end = name + n;

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
            failSemaExpr(expr, ": Binary op on different types");
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
          failSemaExpr(expr, "Only char pointer subtract supported");
        }

        expr->type = getIPtr();
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
      checkBool(binExpr->lhs);
      checkBool(binExpr->rhs);
      expr->type = binExpr->lhs->type;
      return;

    default:
      break;
  }

  if (isAssign(binExpr->op)) {
    let conv = doConvert(state, binExpr->rhs, binExpr->lhs->type);
    if (conv == null) {
      failSemaExpr(expr, ": Assign doesn't match");
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
        failSemaExpr(expr, ": type mismatch");
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
          failSemaExpr(expr, "Parent type not found");
        }
        let unionDecl = parentDecl->kind as DeclKind::Union*;
        if (unionDecl == null) {
          failSemaExpr(expr, "Expected union type");
        }
        typeDecl = findType(unionDecl->subTypes, structExpr.identifier);
      } else {
        typeDecl = lookupType(state, structExpr.identifier);
      }
      if (typeDecl == null || &typeDecl->kind as DeclKind::Struct* == null) {
        failSemaExpr(expr, "Expected struct type for struct init expression");
      }

      // TODO: verify field completeness.
      for (let field = structExpr.fieldIndices; field != null; field = field->next) {
        let fieldDecl = findField(typeDecl, field->fieldName, &field->index);
        if (fieldDecl == null) {
          failSemaExpr(field->value, " cannot find field");
        }

        semaExpr(state, field->value);
        let conv = doConvert(state, field->value, fieldDecl->type);
        if (conv == null) {
          failSemaExpr(field->value, "cannot convert to field type");
        }
        field->value = conv;
      }

      expr->type = typeDecl->type;

    case ExprKind::Scope as scopeExpr:
      let decl = lookupType(state, scopeExpr.parent);
      if (decl == null) {
        failSemaExpr(expr, "Unknown type for scope expr");
      }

      switch (decl->kind) {
        case DeclKind::Enum:
          let fieldDecl = findField(decl, scopeExpr.identifier, &scopeExpr.enumValue);
          if (fieldDecl == null) {
            failSemaExpr(expr, " Cannot find field");
          }

        default:
          failSemaExpr(expr, "Expected enum type for scope expr");
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
          failSemaExpr(expr, ": Expected pointer to struct type for -> expr");
        }
        structDecl = lookupStruct(state, structType);
      } else if (memberExpr.op.kind == TokenKind::DOT) {
        let structType = memberExpr.object->type->kind as TypeKind::Struct*;
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

      let fieldDecl = findField(structDecl, memberExpr.identifier, &memberExpr.fieldIndex);
      if (fieldDecl == null) {
        failSemaExpr(expr, " Cannot find field");
      }

      expr->type = fieldDecl->type;

    case ExprKind::Call as callExpr:
      semaExpr(state, callExpr.function);

      if (callExpr.function == null) {
        failSemaExpr(expr, "Function callee null");
      }
      if (callExpr.function->type == null) {
        failSemaExpr(expr, "Function callee type null");
      }

      // We don't support function pointers
      let funType = callExpr.function->type->kind as TypeKind::Func*;
      if (funType == null) {
        failSemaExpr(expr, "Must call function type");
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
            failSemaExpr(expr, " Arg type mismatch");
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
        }
        last = &cur->next;

        if (curArgTy != null) {
          curArgTy = curArgTy->next;
        }
      }

      let isValidVararg = funType->isVarargs && curArgTy == null;
      if (!isValidVararg && (curArgTy == null) != (cur == null)) {
        failSemaExpr(expr, "Function call arg length mismatch");
      }
      expr->type = funType->result;

    case ExprKind::Conditional as condExpr:
      semaExpr(state, condExpr.cond);
      checkBool(condExpr.cond);
      semaExpr(state, condExpr.trueExpr);
      semaExpr(state, condExpr.falseExpr);

      if (!typeEq(condExpr.trueExpr->type, condExpr.falseExpr->type)) {
        failSemaExpr(expr, "?: lhs and rhs should have same type");
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
          failSemaExpr(expr, "Init must have consistent type");
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
        failSemaExpr(expr, "Couldn't find variable in scope");
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
        failSemaExpr(expr, "Expected int type to be set during parsing.");
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
        failSemaExpr(expr, " Index only works on arrays, or pointers to them.");
      }

      semaExpr(state, indexExpr.index);
      if (indexExpr.index->type->kind as TypeKind::Int* == null) {
        failSemaExpr(expr, "Can't index with non integer");
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
          failSemaExpr(expr, "Expected pointer type for *");
        }
        expr->type = ptrType->pointee;
      }

      // TODO: correct?
      if (unaryExpr.op.kind == TokenKind::BANG) {
        expr->type = getBool();
      }

    case ExprKind::Sizeof as sizeofExpr:
      resolveTypeTags(state, sizeofExpr.typeArg, expr->location);
      sizeofExpr.value = getSize(state, sizeofExpr.typeArg);

      expr->kind = ExprKind::Int {
        value = sizeofExpr.value,
      };
      expr->type = getUPtr();

    case ExprKind::Cast as castExpr:
      semaExpr(state, castExpr.expr);
      if (!semaCast(state, expr)) {
        failSemaExpr(expr, " Can't cast");
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
          failSemaExpr(expr, "Only let expressions allowed");
      }
      if (init == null) {
        failSemaExpr(expr, "Let expression must have an init");
      }
      resolveTypeTags(state, letExpr.decl->type, expr->location);
      semaVarDecl(state, letExpr.decl);
      expr->type = letExpr.decl->type;
  }
}

func evalConstant(state: SemaState*, expr: ExprAST*) -> ExprAST* {
  switch (expr->kind) {
    case ExprKind::Int as intExpr:
      // Already a constant
      return expr;

    case ExprKind::Scope as scopeExpr:
      // Enum values - already a constant
      return expr;

    case ExprKind::Binary as binary:
      let lhs = evalConstant(state, binary.lhs);
      let rhs = evalConstant(state, binary.rhs);

      if (let lhsInt = lhs->kind as ExprKind::Int*) {
        if (let rhsInt = rhs->kind as ExprKind::Int*) {
          let result: i32 = 0;
          switch (binary.op.kind) {
            case TokenKind::PLUS:
              result = lhsInt->value + rhsInt->value;
            case TokenKind::MINUS:
              result = lhsInt->value - rhsInt->value;
            case TokenKind::STAR:
              result = lhsInt->value * rhsInt->value;
            case TokenKind::SLASH:
              if (rhsInt->value == 0) {
                failSemaExpr(expr, "Division by zero in constant expression");
              }
              result = lhsInt->value / rhsInt->value;
            case TokenKind::PERCENT:
              if (rhsInt->value == 0) {
                failSemaExpr(expr, "Modulo by zero in constant expression");
              }
              result = lhsInt->value % rhsInt->value;
            case TokenKind::AND:
              result = lhsInt->value & rhsInt->value;
            case TokenKind::PIPE:
              result = lhsInt->value | rhsInt->value;
            case TokenKind::HAT:
              result = lhsInt->value ^ rhsInt->value;
            case TokenKind::LEFT_OP:
              result = lhsInt->value << rhsInt->value;
            case TokenKind::RIGHT_OP:
              result = lhsInt->value >> rhsInt->value;
            default:
              // Not a constant binary expression
              return expr;
          }

          // Create new constant expression with computed value
          let constExpr = newExpr(ExprKind::Int {
            value = result,
            token = binary.op,
          });
          constExpr->location = expr->location;
          constExpr->type = expr->type;
          return constExpr;
        }
      }
      return expr;

    default:
      // Not a constant expression
      return expr;
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
      failSemaDecl(decl, ": Decl init type doesn't match");
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
            failSemaDecl(decl, "Const decl must have an int init");
        }

      default:
        break;
    }
  } else if (&decl->kind as DeclKind::Const* != null) {
    failSemaDecl(decl, "Const decl must have an init");
  }
}

func resolveTypeTags(state: SemaState*, type: Type*, loc: SourceLoc) {
  if (type == null) {
    return;
  }

  switch (type->kind) {
    case TypeKind::Tag as tagType:
      if (tagType.parent.kind != TokenKind::TOK_EOF) {
        let parentDecl = lookupType(state, tagType.parent);
        if (parentDecl == null) {
          failSema(loc, "Can't resolve type tags, unknown parent type");
        }

        let tagDecl = findType((&parentDecl->kind as DeclKind::Union*)->subTypes, tagType.tag);
        if (tagDecl == null) {
          failSema(loc, "Can't resolve type tags, unknown sub type");
        }

        let next = type->next;
        *type = *tagDecl->type;
        type->next = next;
      } else {
        let typeDecl = lookupType(state, tagType.tag);
        if (typeDecl == null) {
          failSema(loc, "Can't resolve type tags, unknown type");
        }
        type->kind = typeDecl->type->kind;
      }

    case TypeKind::Pointer as p:
      resolveTypeTags(state, p.pointee, loc);
    case TypeKind::Array as a:
      resolveTypeTags(state, a.element, loc);
    case TypeKind::Func as f:
      resolveTypeTags(state, f.result, loc);
      resolveTypeTags(state, f.args, loc);

    case TypeKind::Typeof as typeofType:
      semaExpr(state, typeofType.expr);

      let next = type->next;
      *type = *typeofType.expr->type;
      type->next = next;

    // TODO: is struct parent needed?
    default:
      break;
  }

  resolveTypeTags(state, type->next, loc);
}
