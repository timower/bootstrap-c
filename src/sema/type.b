import ast;

import state;
import sema.lsp;

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
        return ar1.size == ar2->size && typeEq(ar1.element, ar2->element);
      }
      return false;

    case TypeKind::Pointer as ptr1:
      if (let ptr2 = two->kind as TypeKind::Pointer*) {
        return typeEq(ptr1.pointee, ptr2->pointee);
      }
      return false;

    case TypeKind::Slice as s1:
      if (let s2 = two->kind as TypeKind::Slice*) {
        return typeEq(s1.element, s2->element);
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
    case TypeKind::Tag as tag:
      unreachable("Type tag not resolved before eq");
      return true;
    case TypeKind::Typeof as t:
      unreachable("Typeof not resolved before eq");
      return true;
  }
}

func findTypeIdx(types: DeclList*, tag: Token, idxOut: i32*) -> DeclAST* {
  for (let idx = 0; types != null; types = types->next, idx++) {
    let typeTag = getTypeTag(types->decl->type);
    if (tokCmp(tag, *typeTag)) {
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

func findSubType(
    state: SemaState*,
    unionKind: DeclKind::Union*,
    tag: Token,
    idxOut: i32*
) -> DeclAST* {
  if (unionKind == null) {
    return null;
  }
  let res = findTypeIdx(unionKind->subTypes, tag, idxOut);
  if (res != null && state->semaLspMode) {
    lspRef(res, &tag);
  }
  return res;
}

func lookupType(state: SemaState*, tag: Token) -> DeclAST* {
  for (; state != null; state = state->parent) {
    let type = findType(state->types, tag);
    if (type != null) {
      if (state->semaLspMode) {
        lspRef(type, &tag);
      }

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


func getStructDeclSize(state: SemaState*, decl: DeclAST*, parents: DeclList*) -> i32 {
  for (let cur = parents; cur != null; cur = cur->next) {
    if (cur->decl == decl) {
      failSemaDecl(state, decl, "Recursive type declaration!");
    }
  }

  let newParents = newDeclList(decl);
  newParents->next = parents;
  let structKind = decl->kind as DeclKind::Struct*;

  let size = 0;
  for (let field = structKind->fields; field != null; field = field->next) {
    size += getSize(state, field->type, newParents);
  }
  return size == 0 ? 1 : size;
}

func getSize(state: SemaState*, type: Type*, parents: DeclList*) -> i32 {
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

    case TypeKind::Pointer, TypeKind::Func:
      return getPtrSize(&state->target);

    case TypeKind::Slice:
      return 2 * getPtrSize(&state->target);

    case TypeKind::Array as arr:
      if (arr.size < 0) {
        unreachable("Unsized array in sizeof");
      }
      return arr.size * getSize(state, arr.element, parents);

    // TODO: padding
    case TypeKind::Struct as s:
      let decl = lookupType(state, s.tag);
      if (decl == null) {
        failSemaType(state, type, "Unkown type to get size of");
      }

      return getStructDeclSize(state, decl, parents);

    case TypeKind::Union as u:
      let maxSize = 0;
      let decl = lookupType(state, u.tag);
      if (decl == null) {
        failSemaType(state, type, "Unkown type to get size of");
      }

      for (let sub = (&decl->kind as DeclKind::Union*)->subTypes; sub != null; sub = sub->next) {
        let size = getStructDeclSize(state, sub->decl, parents);
        if (size > maxSize) {
          maxSize = size;
        }
      }
      return maxSize + 4;      // i32 tag.

    case TypeKind::Typeof:
      unreachable("Typeof not resolved before getSize");
    case TypeKind::Tag:
      unreachable("Type not resolved before getSize");
  }

  return 0;
}

func isUnsized(type: Type*) -> bool {
  switch (type->kind) {
    case TypeKind::Array as array:
      return array.size < 0 || isUnsized(array.element);
    case TypeKind::Pointer as ptr:
      return isUnsized(ptr.pointee);
    case TypeKind::Slice as sl:
      return isUnsized(sl.element);
    default:
      return false;
  }
}


// TODO: do this on 'doConvert'?
func sizeArrayTypes(state: SemaState*, declType: Type*, initType: Type*) {
  switch (declType->kind) {
    case TypeKind::Array as array:
      let initArray = initType->kind as TypeKind::Array*;
      if (initArray == null) {
        failSemaType(state, declType, "Expected array init for array declaration");
      }
      array.size = initArray->size;
      if (array.size < 0) {
        unreachable("Couldn't infer array size");
      }
      sizeArrayTypes(state, array.element, initArray->element);

    case TypeKind::Pointer as ptr:
      let ptrInit = initType->kind as TypeKind::Pointer*;
      if (ptrInit == null) {
        failSemaType(state, declType, "Expected pointer init for pointer declaration");
      }
      sizeArrayTypes(state, ptr.pointee, ptrInit->pointee);
    default:
      break;
  }
}
