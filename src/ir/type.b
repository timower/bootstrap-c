import ast;

func needsAlloc(expr: ExprAST*) -> bool {
  switch (expr->kind) {
    case ExprKind::Struct, ExprKind::Array, ExprKind::SliceIndex:
      return false;
    case ExprKind::Call:
      return !isAggregate(expr->type);
    case ExprKind::Cast as c:
      return c.castKind != CastKind::StructUnion;
    default:
      return true;
  }
}

func isAggregate(type: Type*) -> bool {
  switch (type->kind) {
    // slice is a {ptr, i32} pair, so aggregate.
    case TypeKind::Struct, TypeKind::Array, TypeKind::Union, TypeKind::Slice:
      return true;
    default:
      return false;
  }
}


// Convert type to LLVM type.
func convertType(type: Type*) -> [i8] {
  if (type == null) {
    return "NULL-TYPE!";
  }
  switch (type->kind) {
    case TypeKind::Void:
      return "void";

    case TypeKind::Bool:
      return "i1";

    case TypeKind::Int as int:
      let buf = malloc(16) as i8*;
      let len = sprintf(buf, "i%d", int.size);
      return buf[:len];

    case TypeKind::Pointer:
      return "ptr";

    case TypeKind::Slice as s:
      return "%slice";

    case TypeKind::Struct as s:
      let len = s.tag.data.len as iptr;
      let buf: [i8] = nullBuf();
      if (s.parent != null) {
        let parent = s.parent->kind as TypeKind::Union*;
        let parentLen = parent->tag.data.len as iptr;
        buf = malloc((len + parentLen + 10) as uptr) as i8[0]*;
        len = sprintf(
            &buf[0],
            "%%struct.%.*s.%.*s",
            parentLen,
            &parent->tag.data[0],
            len,
            &s.tag.data[0]);
      } else {
        buf = malloc((len + 10) as uptr) as i8[0]*;
        len = sprintf(&buf[0], "%%struct.%.*s", len, &s.tag.data[0]);
      }
      return buf[:len];

    case TypeKind::Union as u:
      let len = u.tag.data.len as iptr;
      let buf = malloc((len + 10) as uptr) as i8*;
      len = sprintf(buf, "%%union.%.*s", len, &u.tag.data[0]);
      return buf[:len];

    case TypeKind::Array as arr:
      let buf = malloc(32) as i8*;
      let len = sprintf(buf, "[%d x %s]", arr.size, &convertType(arr.element)[0]);
      return buf[:len];

    case TypeKind::Func as fn:
      let buf = newBuf(128);
      let offset = sprintf(&buf[0], "%s (", &convertType(fn.result)[0]);
      for (let arg = fn.args; arg != null; arg = arg->next) {
        offset += sprintf(&buf[offset], "%s", &convertType(arg)[0]);
        if (arg->next != null) {
          offset += sprintf(&buf[offset], ", ");
        }
      }
      if (fn.isVarargs) {
        offset += sprintf(&buf[offset], ", ...");
      }
      offset += sprintf(&buf[offset], ")");
      return buf[:offset];

    case TypeKind::Enum:
      return "i32";

    case TypeKind::Tag:
      fprintf(getStderr(), "Unknown type to convert");
      exit(1);

    case TypeKind::Typeof:
      fprintf(getStderr(), "Typeof not resolved before IR generation");
      exit(1);
  }

  return nullBuf();
}
