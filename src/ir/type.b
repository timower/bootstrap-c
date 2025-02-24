import ast;

func isAggregate(type: Type*) -> bool {
  return type->kind as TypeKind::Struct* != null
      || type->kind as TypeKind::Union* != null;
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

    case TypeKind::Struct as s:
      let len = s.tag.end - s.tag.data;
      let buf: i8* = null;
      if (s.parent != null) {
        let parent = s.parent->kind as TypeKind::Union*;
        let parentLen = parent->tag.end - parent->tag.data;
        buf = malloc((len + parentLen + 10) as u64);
        sprintf(
            buf,
            "%%struct.%.*s.%.*s",
            parentLen,
            parent->tag.data,
            len,
            s.tag.data);
      } else {
        buf = malloc((len + 10) as u64);
        sprintf(buf, "%%struct.%.*s", len, s.tag.data);
      }
      return buf;

    case TypeKind::Union as u:
      let len = u.tag.end - u.tag.data;
      let buf: i8* = malloc((len + 10) as u64);
      sprintf(buf, "%%union.%.*s", len, u.tag.data);
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
      dprintf(STDERR, "Unknown type to convert");
      exit(1);
  }

  return null;
}
