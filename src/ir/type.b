import ast;

func isAggregate(type: Type*) -> bool {
  // slice is a {ptr, i32} pair, so aggregate.
  return type->kind as TypeKind::Struct* != null
      || type->kind as TypeKind::Union* != null
      || type->kind as TypeKind::Slice* != null;
}


// Convert type to LLVM type.
func convertType(type: Type*) -> const i8* {
  if (type == null) {
    return "NULL-TYPE!";
  }
  switch (type->kind) {
    case TypeKind::Void:
      return "void";

    case TypeKind::Bool:
      return "i1";

    case TypeKind::Int as int:
      let buf: i8* = malloc(16);
      sprintf(buf, "i%d", int.size);
      return buf;

    case TypeKind::Pointer:
      return "ptr";

    case TypeKind::Slice as s:
      return "{ ptr, i32 }";

    case TypeKind::Struct as s:
      let len = s.tag.len as iptr;
      let buf: i8* = null;
      if (s.parent != null) {
        let parent = s.parent->kind as TypeKind::Union*;
        let parentLen = parent->tag.len as iptr;
        buf = malloc((len + parentLen + 10) as uptr);
        sprintf(
            buf,
            "%%struct.%.*s.%.*s",
            parentLen,
            parent->tag.location->data,
            len,
            s.tag.location->data);
      } else {
        buf = malloc((len + 10) as uptr);
        sprintf(buf, "%%struct.%.*s", len, s.tag.location->data);
      }
      return buf;

    case TypeKind::Union as u:
      let len = u.tag.len as iptr;
      let buf: i8* = malloc((len + 10) as uptr);
      sprintf(buf, "%%union.%.*s", len, u.tag.location->data);
      return buf;

    case TypeKind::Array as arr:
      let buf: i8* = malloc(32);
      sprintf(buf, "[%d x %s]", arr.size, convertType(arr.element));
      return buf;

    case TypeKind::Func as fn:
      let buf: i8* = malloc(128);
      let cur = buf + sprintf(buf, "%s (", convertType(fn.result));
      for (let arg = fn.args; arg != null; arg = arg->next) {
        cur += sprintf(cur, "%s", convertType(arg));
        if (arg->next != null) {
          cur += sprintf(cur, ", ");
        }
      }
      if (fn.isVarargs) {
        cur += sprintf(cur, ", ...");
      }
      sprintf(cur, ")");
      return buf;

    case TypeKind::Enum:
      return "i32";

    case TypeKind::Tag:
      fprintf(getStderr(), "Unknown type to convert");
      exit(1);

    case TypeKind::Typeof:
      fprintf(getStderr(), "Typeof not resolved before IR generation");
      exit(1);
  }

  return null;
}
