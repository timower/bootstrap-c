import ast;

func getTypeTag(type: Type*) -> Token* {
  switch (type->kind) {
    case TypeKind::Struct as s:
      return &s.tag;
    case TypeKind::Union as u:
      return &u.tag;
    case TypeKind::Enum as e:
      return &e.tag;
    default:
      unreachable("Type doesn't have a tag");
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
      return *getTypeTag(cur->value);
    }
  }
  return tag;
}

func newTypeMap() -> TypeMap* {
  return calloc(1, sizeof(TypeMap)) as TypeMap*;
}

func substituteTypeWithMapping(type: Type*, mapping: TypeMap*) -> Type* {
  if (type == null) {
    return null;
  }

  let result: TypeKind = type->kind;
  switch (type->kind) {
    case TypeKind::Tag as tag:
      // Use findType to look up this tag in our mapping
      let foundType = lookupTypeMap(mapping, &tag);
      if (foundType != null) {
        result = foundType->kind;
      }

    case TypeKind::Pointer as ptr:
      result = TypeKind::Pointer {
        pointee = substituteTypeWithMapping(ptr.pointee, mapping),
      };

    case TypeKind::Array as arr:
      result = TypeKind::Array {
        element = substituteTypeWithMapping(arr.element, mapping),
        size = arr.size,
      };

    case TypeKind::Slice as s:
      result = TypeKind::Slice {
        element = substituteTypeWithMapping(s.element, mapping),
      };

    case TypeKind::Func as funcType:
      result = TypeKind::Func {
        result = substituteTypeWithMapping(funcType.result, mapping),
        args = substituteTypeWithMapping(funcType.args, mapping),
        isVarargs = funcType.isVarargs,
        typeArgs = substituteTypeWithMapping(funcType.typeArgs, mapping),
      };

    case TypeKind::Typeof as typeOf:
      result = TypeKind::Typeof {
        expr = monomorphizeExpr(typeOf.expr, mapping),
      };

    case TypeKind::Void, TypeKind::Bool, TypeKind::Int, TypeKind::Enum,
         TypeKind::Struct, TypeKind::Union:
      break;
  }

  let resultType = newType(result);
  resultType->next = substituteTypeWithMapping(type->next, mapping);
  return resultType;
}

func getTypeMap(fnType: TypeKind::Func*, callTypeArgs: Type*) -> TypeMap* {
  if (fnType == null || callTypeArgs == null) {
    unreachable("Null fnType or callTypeArgs");
    return null;
  }

  let fnTypeArgs = fnType->typeArgs;

  let typeMapping: TypeMap* = null;

  // Build mapping by matching fnTypeArgs to callTypeArgs
  let fnParam = fnTypeArgs;
  let callArg = callTypeArgs;
  while (fnParam != null && callArg != null) {
    let fnParamTag = &fnParam->kind as TypeKind::Tag*;

    let pair = newTypeMap();
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

func monomorphizeExpr(expr: ExprAST*, typeMap: TypeMap*) -> ExprAST* {
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
        typeArgs = substituteTypeWithMapping(g.typeArgs, typeMap),
      };

    case ExprKind::Sizeof as s:
      resultKind = ExprKind::Sizeof {
        typeArg = substituteTypeWithMapping(s.typeArg, typeMap),
        value = s.value,
      };

    // Trivial cases
    case ExprKind::Array as a:
      resultKind = ExprKind::Array {
        elements = monomorphizeExpr(a.elements, typeMap),
      };
    case ExprKind::Call as c:
      resultKind = ExprKind::Call {
        function = monomorphizeExpr(c.function, typeMap),
        args = monomorphizeExpr(c.args, typeMap),
      };
    case ExprKind::Index as i:
      resultKind = ExprKind::Index {
        array = monomorphizeExpr(i.array, typeMap),
        index = monomorphizeExpr(i.index, typeMap),
      };

    case ExprKind::SliceIndex as i:
      resultKind = ExprKind::SliceIndex {
        slice = monomorphizeExpr(i.slice, typeMap),
        start = monomorphizeExpr(i.start, typeMap),
        end = monomorphizeExpr(i.end, typeMap),
      };

    case ExprKind::Member as m:
      resultKind = ExprKind::Member {
        object = monomorphizeExpr(m.object, typeMap),
        identifier = m.identifier,
        op = m.op,
        fieldIndex = m.fieldIndex,
      };
    case ExprKind::Unary as u:
      resultKind = ExprKind::Unary {
        op = u.op,
        postfix = monomorphizeExpr(u.postfix, typeMap),
        prefix = monomorphizeExpr(u.prefix, typeMap),
      };
    case ExprKind::Conditional as cond:
      resultKind = ExprKind::Conditional {
        cond = monomorphizeExpr(cond.cond, typeMap),
        trueExpr = monomorphizeExpr(cond.trueExpr, typeMap),
        falseExpr = monomorphizeExpr(cond.falseExpr, typeMap),
      };
    case ExprKind::Binary as b:
      resultKind = ExprKind::Binary {
        op = b.op,
        lhs = monomorphizeExpr(b.lhs, typeMap),
        rhs = monomorphizeExpr(b.rhs, typeMap),
      };
    case ExprKind::Cast as c:
      resultKind = ExprKind::Cast {
        expr = monomorphizeExpr(c.expr, typeMap),
        castKind = c.castKind,
        fieldIndex = c.fieldIndex,
      };
    case ExprKind::Paren as p:
      resultKind = ExprKind::Paren {
        expr = monomorphizeExpr(p.expr, typeMap),
      };
    case ExprKind::Let as l:
      resultKind = ExprKind::Let {
        decl = monomorphizeDecls(l.decl, typeMap),
      };
    case ExprKind::Int, ExprKind::Str, ExprKind::Variable:
      break;
  }

  let result = newExpr(resultKind);
  result->location = expr->location;
  result->type = substituteTypeWithMapping(expr->type, typeMap);

  result->next = monomorphizeExpr(expr->next, typeMap);
  return result;
}

func monomorphizeDecls(decl: DeclAST*, typeMap: TypeMap*) -> DeclAST* {
  if (decl == null) {
    return null;
  }

  let varKind = decl->kind as DeclKind::Var*;
  if (varKind == null) {
    printLoc(decl->location);
    fprintf(getStderr(), "TODO: monomorphizeDecls for non var kinds");
    exit(1);
  }

  let result = newDecl(DeclKind::Var {
    init = monomorphizeExpr(varKind->init, typeMap),
    isExtern = varKind->isExtern,
  });
  result->name = decl->name;
  result->location = decl->location;
  result->endLocation = decl->endLocation;

  result->type = substituteTypeWithMapping(decl->type, typeMap);

  result->next = monomorphizeDecls(decl->next, typeMap);
  return result;
}

func monomorphizeStmt(stmt: StmtAST*, typeMap: TypeMap*) -> StmtAST* {
  if (stmt == null) {
    return null;
  }

  let resultKind: StmtKind = StmtKind::Break {};
  switch (stmt->kind) {
    case StmtKind::Compound as c:
      resultKind = StmtKind::Compound {
        stmt = monomorphizeStmt(c.stmt, typeMap),
      };
    case StmtKind::Expr as e:
      resultKind = StmtKind::Expr {
        expr = monomorphizeExpr(e.expr, typeMap),
      };
    case StmtKind::Return as r:
      resultKind = StmtKind::Return {
        expr = monomorphizeExpr(r.expr, typeMap),
      };

    case StmtKind::For as f:
      resultKind = StmtKind::For {
        init = monomorphizeStmt(f.init, typeMap),
        cond = monomorphizeStmt(f.cond, typeMap),
        update = monomorphizeExpr(f.update, typeMap),
        body = monomorphizeStmt(f.body, typeMap),
      };

    case StmtKind::If as i:
      resultKind = StmtKind::If {
        cond = monomorphizeExpr(i.cond, typeMap),
        thenStmt = monomorphizeStmt(i.thenStmt, typeMap),
        elseStmt = monomorphizeStmt(i.elseStmt, typeMap),
      };

    case StmtKind::While as w:
      resultKind = StmtKind::While {
        cond = monomorphizeExpr(w.cond, typeMap),
        body = monomorphizeStmt(w.body, typeMap),
      };

    case StmtKind::Switch as s:
      resultKind = StmtKind::Switch {
        expr = monomorphizeExpr(s.expr, typeMap),
        body = monomorphizeStmt(s.body, typeMap),
      };

    case StmtKind::Case as c:
      resultKind = StmtKind::Case {
        expr = monomorphizeExpr(c.expr, typeMap),
        body = monomorphizeStmt(c.body, typeMap),
      };

    case StmtKind::Default as d:
      resultKind = StmtKind::Default {
        body = monomorphizeStmt(d.body, typeMap),
      };

    case StmtKind::Continue:
      resultKind = StmtKind::Continue {};

    case StmtKind::Break:
      resultKind = StmtKind::Break {};
  }

  let result = newStmt(resultKind);
  result->location = stmt->location;
  result->endLocation = stmt->endLocation;

  result->next = monomorphizeStmt(stmt->next, typeMap);
  return result;
}

func monomorphize(function: DeclAST*, typeMap: TypeMap*, name: Token) -> DeclAST* {
  let newFunc = newDecl(function->kind);
  newFunc->location = function->location;
  newFunc->endLocation = function->endLocation;

  let genericType = function->type->kind as TypeKind::Func*;
  let genericArgs = genericType->typeArgs;

  // Assign new name based on type args.
  newFunc->name = name;

  // Erase genericArgs, as it's not a generic function anymore.
  newFunc->type = substituteTypeWithMapping(function->type, typeMap);
  let funcType = newFunc->type->kind as TypeKind::Func*;
  funcType->typeArgs = null;

  let newFuncKind = newFunc->kind as DeclKind::Func*;
  let funcKind = function->kind as DeclKind::Func*;
  if (newFuncKind == null || funcKind == null) {
    unreachable("Expected function declaration and type");
    return null;
  }

  newFuncKind->args = monomorphizeDecls(funcKind->args, typeMap);
  newFuncKind->body = monomorphizeStmt(funcKind->body, typeMap);
  return newFunc;
}
