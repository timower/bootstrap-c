import ast;

func getTypeTag(type: Type*) -> Token* {
  switch (type->kind) {
    // opt: The type tags are at the same address
    case TypeKind::Struct as s:
      return &s.tag;

    // opt: The type tags are at the same address
    case TypeKind::Union as u:
      return &u.tag;

    // opt: The type tags are at the same address
    case TypeKind::Enum as e:
      return &e.tag;
    default:
      return null;
  }
}

struct TypeMap {
  tag: TypeKind::Tag*;
  value: Type*;

  next: TypeMap*;
}


struct GenericInst {
  // Function that is instantiated.
  function: DeclAST*;
  typeMap: TypeMap*;
  name: Token;

  next: GenericInst*;
}

func lookupTypeMap(map: TypeMap*, tag: TypeKind::Tag*) -> Type* {
  for (let cur = map; cur != null; cur = cur->next) {
    if (tokCmp(cur->tag->tag, tag->tag)) {
      return cur->value;
    }
  }
  return null;
}

func lookupTagTypeMap(map: TypeMap*, tag: Token) -> Token {
  for (let cur = map; cur != null; cur = cur->next) {
    if (tokCmp(cur->tag->tag, tag)) {
      let tag = getTypeTag(cur->value);
      if (tag != null) {
        return *tag;
      }
    }
  }
  return tag;
}

func newTypeMap(allocator: Allocator*) -> TypeMap* {
  return alloc(allocator, sizeof(TypeMap)) as TypeMap*;
}

func substituteTypeWithMapping(
    allocator: Allocator*,
    type: Type*,
    mapping: TypeMap*
) -> Type* {
  if (type == null) {
    return null;
  }

  let result: TypeKind = type->kind;
  switch (type->kind) {
    case TypeKind::Tag as tag:
      if (tag.parent.kind != TokenKind::TOK_EOF) {
        result = TypeKind::Tag {
          parent = lookupTagTypeMap(mapping, tag.parent),
          tag = tag.tag,
        };
      } else {
        let foundType = lookupTypeMap(mapping, &tag);
        if (foundType != null) {
          result = foundType->kind;
        }
      }

    case TypeKind::Pointer as ptr:
      result = TypeKind::Pointer {
        pointee = substituteTypeWithMapping(allocator, ptr.pointee, mapping),
      };

    case TypeKind::Array as arr:
      result = TypeKind::Array {
        element = substituteTypeWithMapping(allocator, arr.element, mapping),
        size = arr.size,
      };

    case TypeKind::Slice as s:
      result = TypeKind::Slice {
        element = substituteTypeWithMapping(allocator, s.element, mapping),
      };

    case TypeKind::Func as funcType:
      result = TypeKind::Func {
        result = substituteTypeWithMapping(allocator, funcType.result, mapping),
        args = substituteTypeWithMapping(allocator, funcType.args, mapping),
        isVarargs = funcType.isVarargs,
        typeArgs = substituteTypeWithMapping(allocator, funcType.typeArgs, mapping),
      };

    case TypeKind::Typeof as typeOf:
      result = TypeKind::Typeof {
        expr = monomorphizeExpr(allocator, typeOf.expr, mapping),
      };

    case TypeKind::Void, TypeKind::Bool, TypeKind::Int, TypeKind::Enum,
         TypeKind::Struct, TypeKind::Union:
      break;
  }

  let resultType = newType(allocator, result);
  resultType->next = substituteTypeWithMapping(allocator, type->next, mapping);
  return resultType;
}

func getTypeMap(
    allocator: Allocator*,
    fnType: TypeKind::Func*,
    callTypeArgs: Type*
) -> TypeMap* {
  let fnTypeArgs = fnType->typeArgs;

  let typeMapping: TypeMap* = null;

  // Build mapping by matching fnTypeArgs to callTypeArgs
  let fnParam = fnTypeArgs;
  let callArg = callTypeArgs;
  while (fnParam != null && callArg != null) {
    let fnParamTag = &fnParam->kind as TypeKind::Tag*;

    let pair = newTypeMap(allocator);
    pair->tag = fnParamTag;
    pair->value = callArg;
    pair->next = typeMapping;
    typeMapping = pair;

    fnParam = fnParam->next;
    callArg = callArg->next;
  }

  // Validate that both lists have the same length
  if (fnParam != null || callArg != null) {
    // Lists have different lengths - error
    return null;
  }

  return typeMapping;
}

func monomorphizeExpr(
    allocator: Allocator*,
    expr: ExprAST*,
    typeMap: TypeMap*
) -> ExprAST* {
  if (expr == null) {
    return null;
  }

  let resultKind: ExprKind = expr->kind;

  switch (expr->kind) {
    case ExprKind::Scope as s:
      resultKind = ExprKind::Scope {
        parent = lookupTagTypeMap(typeMap, s.parent),
        identifier = s.identifier,
        enumValue = s.enumValue,
      };

    case ExprKind::Struct as s:
      resultKind = ExprKind::Struct {
        identifier = lookupTagTypeMap(typeMap, s.identifier),
        parent = lookupTagTypeMap(typeMap, s.parent),
        fieldIndices = s.fieldIndices,
      };

    case ExprKind::GenericInstantiation as g:
      resultKind = ExprKind::GenericInstantiation {
        function = g.function,
        typeArgs = substituteTypeWithMapping(allocator, g.typeArgs, typeMap),
      };

    case ExprKind::Sizeof as s:
      resultKind = ExprKind::Sizeof {
        typeArg = substituteTypeWithMapping(allocator, s.typeArg, typeMap),
        value = s.value,
      };

    // Trivial cases
    case ExprKind::Array as a:
      resultKind = ExprKind::Array {
        elements = monomorphizeExpr(allocator, a.elements, typeMap),
      };
    case ExprKind::Call as c:
      resultKind = ExprKind::Call {
        function = monomorphizeExpr(allocator, c.function, typeMap),
        args = monomorphizeExpr(allocator, c.args, typeMap),
      };
    case ExprKind::Index as i:
      resultKind = ExprKind::Index {
        array = monomorphizeExpr(allocator, i.array, typeMap),
        index = monomorphizeExpr(allocator, i.index, typeMap),
      };

    case ExprKind::SliceIndex as i:
      resultKind = ExprKind::SliceIndex {
        slice = monomorphizeExpr(allocator, i.slice, typeMap),
        start = monomorphizeExpr(allocator, i.start, typeMap),
        end = monomorphizeExpr(allocator, i.end, typeMap),
      };

    case ExprKind::Member as m:
      resultKind = ExprKind::Member {
        object = monomorphizeExpr(allocator, m.object, typeMap),
        identifier = m.identifier,
        op = m.op,
        fieldIndex = m.fieldIndex,
      };
    case ExprKind::Unary as u:
      resultKind = ExprKind::Unary {
        op = u.op,
        postfix = monomorphizeExpr(allocator, u.postfix, typeMap),
        prefix = monomorphizeExpr(allocator, u.prefix, typeMap),
      };
    case ExprKind::Conditional as cond:
      resultKind = ExprKind::Conditional {
        cond = monomorphizeExpr(allocator, cond.cond, typeMap),
        trueExpr = monomorphizeExpr(allocator, cond.trueExpr, typeMap),
        falseExpr = monomorphizeExpr(allocator, cond.falseExpr, typeMap),
      };
    case ExprKind::Binary as b:
      resultKind = ExprKind::Binary {
        op = b.op,
        lhs = monomorphizeExpr(allocator, b.lhs, typeMap),
        rhs = monomorphizeExpr(allocator, b.rhs, typeMap),
      };
    case ExprKind::Cast as c:
      resultKind = ExprKind::Cast {
        expr = monomorphizeExpr(allocator, c.expr, typeMap),
        castKind = c.castKind,
        fieldIndex = c.fieldIndex,
      };
    case ExprKind::Paren as p:
      resultKind = ExprKind::Paren {
        expr = monomorphizeExpr(allocator, p.expr, typeMap),
      };
    case ExprKind::Let as l:
      resultKind = ExprKind::Let {
        decl = monomorphizeDecls(allocator, l.decl, typeMap),
      };
    case ExprKind::Int, ExprKind::Str, ExprKind::Variable:
      break;
  }

  let result = newExpr(allocator, resultKind);
  result->location = expr->location;
  result->type = substituteTypeWithMapping(allocator, expr->type, typeMap);

  result->next = monomorphizeExpr(allocator, expr->next, typeMap);
  return result;
}

func monomorphizeDecls(
    allocator: Allocator*,
    decl: DeclAST*,
    typeMap: TypeMap*
) -> DeclAST* {
  if (decl == null) {
    return null;
  }

  let result: DeclAST* = null;
  switch (decl->kind) {
    case DeclKind::Var as var:
      result = newDecl(allocator, DeclKind::Var {
        init = monomorphizeExpr(allocator, var.init, typeMap),
        isExtern = var.isExtern,
      });

    case DeclKind::Const as cst:
      result = newDecl(allocator, DeclKind::Const {
        init = monomorphizeExpr(allocator, cst.init, typeMap),
      });
    default:
      unreachable("Non var decl in function?");
  }

  result->name = decl->name;
  result->location = decl->location;
  result->endLocation = decl->endLocation;

  result->type = substituteTypeWithMapping(allocator, decl->type, typeMap);
  result->next = monomorphizeDecls(allocator, decl->next, typeMap);
  return result;
}

func monomorphizeStmt(
    allocator: Allocator*,
    stmt: StmtAST*,
    typeMap: TypeMap*
) -> StmtAST* {
  if (stmt == null) {
    return null;
  }

  let resultKind: StmtKind = StmtKind::Break {};
  switch (stmt->kind) {
    case StmtKind::Compound as c:
      resultKind = StmtKind::Compound {
        stmt = monomorphizeStmt(allocator, c.stmt, typeMap),
      };
    case StmtKind::Expr as e:
      resultKind = StmtKind::Expr {
        expr = monomorphizeExpr(allocator, e.expr, typeMap),
      };
    case StmtKind::Return as r:
      resultKind = StmtKind::Return {
        expr = monomorphizeExpr(allocator, r.expr, typeMap),
      };

    case StmtKind::For as f:
      resultKind = StmtKind::For {
        init = monomorphizeStmt(allocator, f.init, typeMap),
        cond = monomorphizeStmt(allocator, f.cond, typeMap),
        update = monomorphizeExpr(allocator, f.update, typeMap),
        body = monomorphizeStmt(allocator, f.body, typeMap),
      };

    case StmtKind::If as i:
      resultKind = StmtKind::If {
        cond = monomorphizeExpr(allocator, i.cond, typeMap),
        thenStmt = monomorphizeStmt(allocator, i.thenStmt, typeMap),
        elseStmt = monomorphizeStmt(allocator, i.elseStmt, typeMap),
      };

    case StmtKind::While as w:
      resultKind = StmtKind::While {
        cond = monomorphizeExpr(allocator, w.cond, typeMap),
        body = monomorphizeStmt(allocator, w.body, typeMap),
      };
    case StmtKind::Defer as d:
      resultKind = StmtKind::Defer {
        stmt = monomorphizeStmt(allocator, d.stmt, typeMap),
      };

    case StmtKind::Switch as s:
      resultKind = StmtKind::Switch {
        expr = monomorphizeExpr(allocator, s.expr, typeMap),
        body = monomorphizeStmt(allocator, s.body, typeMap),
      };

    case StmtKind::Case as c:
      resultKind = StmtKind::Case {
        expr = monomorphizeExpr(allocator, c.expr, typeMap),
        body = monomorphizeStmt(allocator, c.body, typeMap),
      };

    case StmtKind::Default as d:
      resultKind = StmtKind::Default {
        body = monomorphizeStmt(allocator, d.body, typeMap),
      };

    case StmtKind::Continue:
      resultKind = StmtKind::Continue {};

    case StmtKind::Break:
      resultKind = StmtKind::Break {};
  }

  let result = newStmt(allocator, resultKind);
  result->location = stmt->location;
  result->endLocation = stmt->endLocation;

  result->next = monomorphizeStmt(allocator, stmt->next, typeMap);
  return result;
}

func monomorphize(
    allocator: Allocator*,
    function: DeclAST*,
    typeMap: TypeMap*,
    name: Token
) -> DeclAST* {
  let newFunc = newDecl(allocator, function->kind);
  newFunc->location = function->location;
  newFunc->endLocation = function->endLocation;

  let genericType = function->type->kind as TypeKind::Func*;
  let genericArgs = genericType->typeArgs;

  // Assign new name based on type args.
  newFunc->name = name;

  // Erase genericArgs, as it's not a generic function anymore.
  newFunc->type = substituteTypeWithMapping(allocator, function->type, typeMap);
  let funcType = newFunc->type->kind as TypeKind::Func*;
  funcType->typeArgs = null;

  let newFuncKind = newFunc->kind as DeclKind::Func*;
  let funcKind = function->kind as DeclKind::Func*;
  if (newFuncKind == null || funcKind == null) {
    unreachable("Expected function declaration and type");
    return null;
  }

  newFuncKind->args = monomorphizeDecls(allocator, funcKind->args, typeMap);
  newFuncKind->body = monomorphizeStmt(allocator, funcKind->body, typeMap);
  return newFunc;
}
