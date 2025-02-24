import irgen.state;

import ir.type;

func genLoad(state: IRGenState*, addr: Value, type: Type*) -> Value {
  if (type->kind as TypeKind::Func* != null
      || type->kind as TypeKind::Array* != null) {
    return addr;
  }
  if (isAggregate(type)) {
    return addr;
  }

  return addInstr(state, type, InstrKind::Load {
    ptr = addr,
  });
}

func genMemcpy(state: IRGenState*, addr: Value, val: Value, type: Type*) {
  let size = Value::Sizeof {
    type = type,
  };
  let fn = Value::FuncPtr {
    ptr = state->intrinsics.memcpy,
  };

  let args = calloc(4, sizeof(union Value)) as Value*;
  *(args + 0) = addr;
  *(args + 1) = val;
  *(args + 2) = size;
  *(args + 3) = Value::IntConstant {
    value = 0,
    type = getBool(),
  };

  addInstr(state, null, InstrKind::Call {
    fn = fn,
    args = args,
    numArgs = 4,
  });
}

func genStore(state: IRGenState*, addr: Value, val: Value, type: Type*) {
  if (isAggregate(type)) {
    genMemcpy(state, addr, val, type);
  } else {
    addInstr(state, null, InstrKind::Store {
      ptr = addr,
      val = val,
    });
  }
}


// This is fine as IR shouldn't use the type of pointers
func getPtrType() -> Type* {
  return newType(TypeKind::Pointer {});
}
