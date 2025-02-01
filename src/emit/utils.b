import state;
import type;

func intToVal(num: i32, type: Type*) -> Value {
  if (let ptrType = type->kind as TypeKind::Pointer*) {
    if (num != 0) {
      failEmit("expected null pointer");
    }
    return Value {
      type = "ptr",
      val = "null",
    };
  }

  let buf: i8* = malloc(16);
  sprintf(buf, "%d", num);
  return Value {
    type = convertType(type),
    val = buf,
  };
}


// Turns an i1 into an i32
func upcasti1(state: EmitState*, val: Value) -> Value {
  let up = getNextTemp(state);
  up.type = val.type;
  if (strcmp(up.type, "ptr") == 0) {
    failEmit("bool to pointer?");
  }
  printf("  %s = zext i1 %s to %s\n", up.val, val.val, up.type);
  return up;
}


// Turns an i32 into an i1
func makeBool(state: EmitState*, val: Value) -> Value {
  let up = getNextTemp(state);
  up.type = val.type;
  printf("  %s = icmp ne %s %s, 0\n", up.val, val.type, val.val);
  return up;
}

func emitStructGEP(
    state: EmitState*,
    aggType: Type*,
    agg: Value,
    idx: i32
) -> Value {
  let gep = getNextTemp(state);
  gep.type = "ptr";
  printf(
      "  %s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d\n",
      gep.val,
      convertType(aggType),
      agg.val,
      idx);
  return gep;
}


func getLLVMSize(type: Type*) -> Value {
  let res = Value {};
  res.val = malloc(128);
  sprintf(
      res.val,
      "ptrtoint (ptr getelementptr (%s, ptr null, i32 1) to i32)",
      convertType(type));
  res.type = "i32";
  return res;
}

func emitMemcpy(addr: Value, val: Value, type: Type*) {
  let size = getLLVMSize(type);
  printf(
      "  call void @llvm.memcpy.p0.p0.i32(ptr %s, ptr %s, %s %s, i1 0)\n",
      addr.val,
      val.val,
      size.type,
      size.val);
}

func emitStore(addr: Value, val: Value, type: Type*) {
  if (isAggregate(type)) {
    emitMemcpy(addr, val, type);
  } else {
    printf("  store %s %s, ptr %s\n", val.type, val.val, addr.val);
  }
}

func emitRawLoad(state: EmitState*, addr: Value, type: Type*) -> Value {
  let val = getNextTemp(state);
  val.type = convertType(type);
  printf("  %s = load %s, ptr %s\n", val.val, val.type, addr.val);
  return val;
}

func emitLoad(state: EmitState*, addr: Value, type: Type*) -> Value {
  // Funcs and arrays are implictly converted to pointers here.
  if (type->kind as TypeKind::Func* != null
      || type->kind as TypeKind::Array* != null) {
    return addr;
  }

  // Structs are stored on stack
  if (isAggregate(type)) {
    return addr;
  }

  return emitRawLoad(state, addr, type);
}

func emitAlloca(state: EmitState*, type: Type*) -> Value {
  let res = getNextTemp(state);
  res.type = "ptr";
  printf("  %s = alloca %s\n", res.val, convertType(type));
  return res;
}

func emitLocalVar(state: EmitState*, decl: DeclAST*) -> Value {
  let val = emitAlloca(state, decl->type);

  let local = newLocal(decl->name, val);
  local->next = state->vars;
  state->vars = local;

  return val;
}
