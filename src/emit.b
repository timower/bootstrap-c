import ir;
import libc;
import ir.print;

struct EmitState {};

func emitAsm(module: Module*) {
  fprintf(outFile, ".text\n");
  fprintf(outFile, ".global _main\n");
  
  for (let fn = module->functions; fn != null; fn = fn->next) {
    emitFunction(fn);
  }
}

func emitFunction(fn: Function*) {
  let name = fn->name;
  if (*name == '@') {
    name = name + 1;
  }
  fprintf(outFile, "_%s:\n", name);
  
  for (let bb = fn->begin; bb != null; bb = bb->next) {
    emitBasicBlock(bb);
  }
}

func emitBasicBlock(bb: BasicBlock*) {
  for (let instr = bb->begin; instr != null; instr = instr->next) {
    emitInstruction(instr);
  }
}

func emitInstruction(instr: Instruction*) {
  switch (instr->kind) {
    case InstrKind::Binary as b:
      emitBinaryOp(instr, b);
    case InstrKind::Return as r:
      switch (r.val) {
        case Value::IntConstant as i:
          fprintf(outFile, "  mov w0, #%d\n", i.value);
        case Value::InstrPtr as p:
          // Return the result of a previous instruction
          fprintf(outFile, "  mov w0, w%d\n", p.ptr->name);
        default:
          // TODO: handle other value types
      }
      fprintf(outFile, "  ret\n");
    case InstrKind::ReturnVoid:
      fprintf(outFile, "  ret\n");
    default:
      // TODO: handle other instructions
  }
}

func emitBinaryOp(instr: Instruction*, b: InstrKind::Binary) {
  let opStr: i8* = null;
  let supportsImmediate = false;
  switch (b.op) {
    case BinaryOp::Add:
      opStr = "add";
      supportsImmediate = true;
    case BinaryOp::Sub:
      opStr = "sub";
      supportsImmediate = true;
    case BinaryOp::Mul:
      opStr = "mul";
      supportsImmediate = false;
    case BinaryOp::SDiv:
      opStr = "sdiv";
      supportsImmediate = false;
    default:
      // TODO: handle other operations
      opStr = "unknown";
      supportsImmediate = false;
  }
  
  // For ARM64, we need to handle immediate operands carefully
  let bothImmediate = isImmediate(b.lhs) && isImmediate(b.rhs);
  let rhsImmediateUnsupported = isImmediate(b.rhs) && !supportsImmediate;
  
  if (bothImmediate) {
    // Load first operand into register, keep second as immediate if supported
    fprintf(outFile, "  mov w%d, ", instr->name);
    emitValue(b.lhs);
    fprintf(outFile, "\n");
    
    if (supportsImmediate) {
      // Can use immediate for second operand
      fprintf(outFile, "  %s w%d, w%d, ", opStr, instr->name, instr->name);
      emitValue(b.rhs);
      fprintf(outFile, "\n");
    } else {
      // Need to load second operand into register too
      let tempReg = 2;
      fprintf(outFile, "  mov w%d, ", tempReg);
      emitValue(b.rhs);
      fprintf(outFile, "\n");
      fprintf(outFile, "  %s w%d, w%d, w%d\n", opStr, instr->name, instr->name, tempReg);
    }
  } else if (rhsImmediateUnsupported) {
    // First operand is register, second is immediate but not supported
    let tempReg = 2;
    fprintf(outFile, "  mov w%d, ", tempReg);
    emitValue(b.rhs);
    fprintf(outFile, "\n");
    fprintf(outFile, "  %s w%d, ", opStr, instr->name);
    emitValue(b.lhs);
    fprintf(outFile, ", w%d\n", tempReg);
  } else {
    // Normal case: opStr w<dest>, <lhs>, <rhs>
    fprintf(outFile, "  %s w%d, ", opStr, instr->name);
    emitValue(b.lhs);
    fprintf(outFile, ", ");
    emitValue(b.rhs);
    fprintf(outFile, "\n");
  }
}

func isImmediate(val: Value) -> bool {
  switch (val) {
    case Value::IntConstant:
      return true;
    default:
      return false;
  }
}

func emitValue(val: Value) {
  switch (val) {
    case Value::IntConstant as i:
      fprintf(outFile, "#%d", i.value);
    case Value::InstrPtr as p:
      fprintf(outFile, "w%d", p.ptr->name);
    default:
      // TODO: handle other value types
      fprintf(outFile, "unknown");
  }
}
