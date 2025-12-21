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
      return "%slice";

    case TypeKind::Struct as s:
      let len = s.tag.data.len as iptr;
      let buf: i8* = null;
      if (s.parent != null) {
        let parent = s.parent->kind as TypeKind::Union*;
        let parentLen = parent->tag.data.len as iptr;
        buf = malloc((len + parentLen + 10) as uptr);
        sprintf(
            buf,
            "%%struct.%.*s.%.*s",
            parentLen,
            &parent->tag.data[0],
            len,
            &s.tag.data[0]);
      } else {
        buf = malloc((len + 10) as uptr);
        sprintf(buf, "%%struct.%.*s", len, &s.tag.data[0]);
      }
      return buf;

    case TypeKind::Union as u:
      let len = u.tag.data.len as iptr;
      let buf: i8* = malloc((len + 10) as uptr);
      sprintf(buf, "%%union.%.*s", len, &u.tag.data[0]);
      return buf;

    case TypeKind::Array as arr:
      let buf: i8* = malloc(32);
      sprintf(buf, "[%d x %s]", arr.size, convertType(arr.element));
      return buf;

    case TypeKind::Func as fn:
      let buf = newBuf(128);
      let offset = sprintf(&buf[0], "%s (", convertType(fn.result));
      for (let arg = fn.args; arg != null; arg = arg->next) {
        offset += sprintf(&buf[offset], "%s", convertType(arg));
        if (arg->next != null) {
          offset += sprintf(&buf[offset], ", ");
        }
      }
      if (fn.isVarargs) {
        offset += sprintf(&buf[offset], ", ...");
      }
      sprintf(&buf[offset], ")");
      return &buf[0];

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
