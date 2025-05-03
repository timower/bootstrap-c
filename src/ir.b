import ast;

struct Module {
  types: IRStruct*;
  globals: Global*;
  functions: Function*;
};

struct IRStruct {
  name: i8*;

  // TODO: figure out type.
  fields: Type*;

  next: IRStruct*;
};

struct Global {
  name: i8*;
  type: Type*;

  isExtern: bool;
  init: Value;

  next: Global*;
};

struct Function {
  name: i8*;
  type: Type*;

  allocs: Alloca*;

  begin: BasicBlock*;
  end: BasicBlock*;

  next: Function*;
};

struct Alloca {
  name: i32;
  type: Type*;

  next: Alloca*;
};

struct BasicBlock {
  label: i8*;
  name: i32;

  begin: Instruction*;
  end: Instruction*;

  next: BasicBlock*;
};

enum BinaryOp {
  Add,
  Sub,
  Mul,
  SDiv,
  SRem,
  Shl,
  AShr,
  LShr,
  And,
  Xor,
  Or,
};

enum CmpOp {
  Eq,
  Ne,
  Slt,
  Sle,
  Sgt,
  Sge,
};

union InstrKind {
  StructGEP {
    type: Type*;
    ptr: Value;
    field: i32;
  }
  ArrayGEP {
    type: Type*;
    ptr: Value;
    idx: Value;
  }

  Binary {
    op: BinaryOp;
    lhs: Value;
    rhs: Value;
  }
  Cmp {
    op: CmpOp;
    lhs: Value;
    rhs: Value;
  }

  Cast {
    kind: CastKind;
    val: Value;
  }

  Call {
    fn: Value;
    args: Value*;
    numArgs: i32;
  }
  Branch {
    bb: BasicBlock*;
  }
  CondBranch {
    cond: Value;
    trueBB: BasicBlock*;
    falseBB: BasicBlock*;
  }
  Switch {
    cond: Value;
    defaultBB: BasicBlock*;
    cases: Case*;
  }

  Phi {
    trueBB: BasicBlock*;
    trueVal: Value;
    falseBB: BasicBlock*;
    falseVal: Value;
  }
  Select {
    cond: Value;
    trueVal: Value;
    falseVal: Value;
  }

  Store {
    ptr: Value;
    val: Value;
  }
  Load {
    ptr: Value;
  }

  Return {
    val: Value;
  }
  ReturnVoid {}
};

struct Case {
  val: Value;
  bb: BasicBlock*;

  next: Case*;
};

struct Instruction {
  name: i32;

  // Can be null for lots of instructions (ret, store, ...)
  type: Type*;

  kind: InstrKind;

  next: Instruction*;
};

union Value {
  InstrPtr {
    ptr: Instruction*;
  }

  GlobalPtr {
    ptr: Global*;
  }

  FuncPtr {
    ptr: Function*;
  }

  IntConstant {
    value: i32;
    type: Type*;
  }

  StrConstant {
    value: Token;
    type: Type*;
  }

  ArrayConstant {
    type: Type*;
    values: Value*;
    size: i32;    // TODO: redundant with (type->kind as Array)->size?
  }

  Argument {
    type: Type*;
    idx: i32;
  }

  AllocaPtr {
    ptr: Alloca*;
  }

  Sizeof {
    type: Type*;
  }

  // null for ptr types, zeroinitializer for aggregates.
  Zero {
    type: Type*;
  }
};


func newFunction() -> Function* {
  return calloc(1, sizeof(struct Function)) as Function*;
}

func newGlobal() -> Global* {
  return calloc(1, sizeof(struct Global)) as Global*;
}

func newIRStruct() -> IRStruct* {
  return calloc(1, sizeof(struct IRStruct)) as IRStruct*;
}
