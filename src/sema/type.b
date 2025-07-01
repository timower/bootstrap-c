import ast;

import state;

func typeEq(one: Type*, two: Type*) -> bool {
  switch (one->kind) {
    case TypeKind::Void:
      return two->kind as TypeKind::Void* != null;
    case TypeKind::Bool:
      return two->kind as TypeKind::Bool* != null;
    case TypeKind::Int as int1:
      if (let int2 = two->kind as TypeKind::Int*) {
        return int1.isSigned == int2->isSigned && int1.size == int2->size;
      }
      return false;

    case TypeKind::Array as ar1:
      if (let ar2 = two->kind as TypeKind::Array*) {
        return (ar1.size < 0 || ar2->size < 0 || ar1.size == ar2->size)
            && typeEq(ar1.element, ar2->element);
      }
      return false;

    case TypeKind::Pointer as ptr1:
      if (let ptr2 = two->kind as TypeKind::Pointer*) {
        return typeEq(ptr1.pointee, ptr2->pointee);
      }
      return false;

    case TypeKind::Struct as s1:
      if (let s2 = two->kind as TypeKind::Struct*) {
        if (s1.parent != null) {
          if (s2->parent == null || !typeEq(s1.parent, s2->parent)) {
            return false;
          }
        }
        return tokCmp(s1.tag, s2->tag);
      }
      return false;

    case TypeKind::Enum as e1:
      if (let e2 = two->kind as TypeKind::Enum*) {
        return tokCmp(e1.tag, e2->tag);
      }
      return false;

    case TypeKind::Union as u1:
      if (let u2 = two->kind as TypeKind::Union*) {
        return tokCmp(u1.tag, u2->tag);
      }
      return false;

    case TypeKind::Func as f1:
      if (let f2 = two->kind as TypeKind::Func*) {
        if (f1.isVarargs != f2->isVarargs || !typeEq(f1.result, f2->result)) {
          return false;
        }

        let arg1 = f1.args;
        let arg2 = f2->args;

        while (arg1 != null && arg2 != null) {
          if (!typeEq(arg1, arg2)) {
            return false;
          }
          arg1 = arg1->next;
          arg2 = arg2->next;
        }

        return arg1 == null && arg2 == null;
      }
      return false;
    case TypeKind::Tag:
      failSema(SourceLoc {}, "Type tag not resolved before eq");
    case TypeKind::Typeof:
      failSema(SourceLoc {}, "Typeof not resolved before eq");
  }

  return true;
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


func getPointerToArray(type: Type*) -> TypeKind::Array* {
  if (let fromPtr = type->kind as TypeKind::Pointer*) {
    if (let fromArray = fromPtr->pointee->kind as TypeKind::Array*) {
      return fromArray;
    }
  }
  return null;
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

func getStructDeclSize(state: SemaState*, decl: DeclAST*) -> i32 {
  let size = 0;
  for (let field = (&decl->kind as DeclKind::Struct*)->fields; field != null; field = field->next) {
    size += getSize(state, field->type);
  }
  return size == 0 ? 1 : size;
}

func getSize(state: SemaState*, type: Type*) -> i32 {
  switch (type->kind) {
    case TypeKind::Void:
      return 0;

    case TypeKind::Bool:
      return 1;

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
      for (let sub = (&decl->kind as DeclKind::Union*)->subTypes; sub != null; sub = sub->next) {
        let size = getStructDeclSize(state, sub->decl);
        if (size > maxSize) {
          maxSize = size;
        }
      }
      return maxSize + 4;      // i32 tag.

    case TypeKind::Typeof:
      failSema(SourceLoc {}, "Typeof not resolved before getSize");
      return 0;

    default:
      printType(type);
      failSema(SourceLoc {}, "Unknown type for size");
      return 0;
  }
}


// TODO: do this on 'doConvert'?
func sizeArrayTypes(declType: Type*, initType: Type*) {
  switch (declType->kind) {
    case TypeKind::Array as array:
      let initArray = initType->kind as TypeKind::Array*;
      array.size = initArray->size;
      if (array.size < 0) {
        failSema(SourceLoc {}, "Coudln't infer array size");
      }
      sizeArrayTypes(array.element, initArray->element);

    case TypeKind::Pointer as ptr:
      let ptrInit = initType->kind as TypeKind::Pointer*;
      sizeArrayTypes(ptr.pointee, ptrInit->pointee);
    default:
      break;
  }
}
