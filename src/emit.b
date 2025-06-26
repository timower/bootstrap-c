import ir;
import libc;
import ir.print;

struct EmitState {
  // Register allocation state
  usedRegs: bool[32];  // Track which registers are in use (w0-w31)

  // Virtual register to physical register mapping
  instrToReg: i32*;  // Map instruction names to physical registers
  instrToRegSize: i32;  // Size of the mapping array
};

func failEmit(msg: i8*) {
  printf("Emit error: %s\n", msg);
  exit(1);
}

func initEmitState(state: EmitState*) {
  // Initialize register tracking
  for (let i = 0; i < 32; i = i + 1) {
    state->usedRegs[i] = false;
  }

  // Reserve special-purpose registers
  state->usedRegs[0] = true;  // w0 reserved for return values
  state->usedRegs[29] = true;  // w29 reserved for frame pointer
  state->usedRegs[30] = true;  // w30 reserved for link register

  // Initialize mapping arrays
  state->instrToReg = null;
  state->instrToRegSize = 0;
}

func setInstrReg(state: EmitState*, instrName: i32, reg: i32) {
  if (instrName >= state->instrToRegSize) {
    failEmit("Instruction name out of bounds in register mapping");
  }
  *(state->instrToReg + instrName) = reg;
}

func getInstrReg(state: EmitState*, instrName: i32) -> i32 {
  if (instrName >= state->instrToRegSize) {
    failEmit("Instruction name out of bounds in register mapping");
  }
  return *(state->instrToReg + instrName);
}

func getPhysicalReg(state: EmitState*, instr: Instruction*) -> i32 {
  // Get the allocated physical register for an instruction
  let reg = getInstrReg(state, instr->name);
  if (reg == -1) {
    failEmit("No register allocated for instruction");
  }

  return reg;
}

func allocateNextRegister(state: EmitState*) -> i32 {
  // Scan usedRegs array to find first available register
  // Starts from one as x0 is always reserved.
  for (let reg = 1; reg < 32; reg = reg + 1) {
    if (!state->usedRegs[reg]) {
      state->usedRegs[reg] = true;
      return reg;
    }
  }
  failEmit("Out of registers!");
}

func freeRegister(state: EmitState*, reg: i32) {
  if (!state->usedRegs[reg]) {
    failEmit("Double reg free, in SSA?");
  }

  state->usedRegs[reg] = false;
}


func allocateRegistersBackwards(state: EmitState*, fn: Function*) {
  // First pass: find the maximum instruction name to size the mapping array
  let maxInstrName = 0;
  for (let bb = fn->begin; bb != null; bb = bb->next) {
    for (let instr = bb->begin; instr != null; instr = instr->next) {
      if (instr->name > maxInstrName) {
        maxInstrName = instr->name;
      }
    }
  }

  // Allocate mapping array
  state->instrToRegSize = maxInstrName + 1;
  let size = (state->instrToRegSize * 4) as i64;
  state->instrToReg = malloc(size as u64) as i32*;

  // Initialize all mappings to -1 (unassigned)
  for (let i = 0; i < state->instrToRegSize; i = i + 1) {
    setInstrReg(state, i, -1);
  }

  // Walk backwards through basic blocks
  for (let bb = fn->end; bb != null; bb = bb->prev) {
    // Walk backwards through instructions in this block
    for (let instr = bb->end; instr != null; instr = instr->prev) {
      processInstructionBackwards(state, instr);
    }
  }
}

func processInstructionBackwards(state: EmitState*, instr: Instruction*) {
  // Process uses first - these will allocate registers via markAsLive
  processUsesBackwards(state, instr);

  // Then process definition - this is where the register lifetime starts (walking backwards)
  // The register should already be allocated by markAsLive, just free it for reuse
  if (hasResult(instr)) {
    freeRegister(state, getPhysicalReg(state, instr));
  }
}

func processUsesBackwards(state: EmitState*, instr: Instruction*) {
  switch (instr->kind) {
    case InstrKind::Binary as b:
      markAsLive(state, b.lhs);
      markAsLive(state, b.rhs);
    case InstrKind::Return as r:
      markAsLive(state, r.val);
    default:
      failEmit("Unhandled instruction kind in processUsesBackwards");
  }
}

func markAsLive(state: EmitState*, val: Value) {
  switch (val) {
    case Value::InstrPtr as p:
      // Walking backwards: this use means we need to allocate a register
      // if one hasn't been allocated yet
      let reg = getInstrReg(state, p.ptr->name);
      if (reg == -1) {
        // First time seeing this value (walking backwards), allocate register
        reg = allocateNextRegister(state);
        setInstrReg(state, p.ptr->name, reg);
      }

    default:
      // Constants, globals, etc. don't need physical registers
      break;
  }
}

func emitAsm(module: Module*, target: i8*) {
  let useUnderscore = strcmp(target, "darwin") == 0;
  fprintf(outFile, ".text\n");
  if (useUnderscore) {
    fprintf(outFile, ".global _main\n");
  } else {
    fprintf(outFile, ".global main\n");
  }

  for (let fn = module->functions; fn != null; fn = fn->next) {
    emitFunction(fn, useUnderscore);
  }
}

func emitFunction(fn: Function*, useUnderscore: bool) {
  let name = fn->name;
  if (*name == '@') {
    name = name + 1;
  }
  if (useUnderscore) {
    fprintf(outFile, "_%s:\n", name);
  } else {
    fprintf(outFile, "%s:\n", name);
  }

  // Create register allocation state for this function
  let state = EmitState {};
  initEmitState(&state);

  // Allocate registers before emission
  allocateRegistersBackwards(&state, fn);

  for (let bb = fn->begin; bb != null; bb = bb->next) {
    emitBasicBlock(&state, bb);
  }
}

func emitBasicBlock(state: EmitState*, bb: BasicBlock*) {
  for (let instr = bb->begin; instr != null; instr = instr->next) {
    emitInstruction(state, instr);
  }
}

func emitInstruction(state: EmitState*, instr: Instruction*) {
  switch (instr->kind) {
    case InstrKind::Binary as b:
      emitBinaryOp(state, instr, b);
    case InstrKind::Return as r:
      fprintf(outFile, "  mov w0, ");
      emitValue(state, r.val);
      fprintf(outFile, "\n");
      fprintf(outFile, "  ret\n");
    case InstrKind::ReturnVoid:
      fprintf(outFile, "  ret\n");

    default:
      // TODO: handle other instructions
      failEmit("Unhandled instruction kind in emitInstruction");
  }
}

func emitBinaryOp(state: EmitState*, instr: Instruction*, b: InstrKind::Binary) {
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
      failEmit("Unhandled binary operation in emitBinaryOp");
  }

  // Get the allocated register for this instruction's result
  let destReg = getPhysicalReg(state, instr);

  // For ARM64, we need to handle immediate operands carefully
  let bothImmediate = isImmediate(b.lhs) && isImmediate(b.rhs);
  let rhsImmediateUnsupported = isImmediate(b.rhs) && !supportsImmediate;

  if (bothImmediate) {
    // Load first operand into destination register, keep second as immediate if supported
    fprintf(outFile, "  mov w%d, ", destReg);
    emitValue(state, b.lhs);
    fprintf(outFile, "\n");

    if (supportsImmediate) {
      // Can use immediate for second operand
      fprintf(outFile, "  %s w%d, w%d, ", opStr, destReg, destReg);
      emitValue(state, b.rhs);
      fprintf(outFile, "\n");
    } else {
      // Need to load second operand into a temp register
      // For now, use a hardcoded temp register - this should be improved
      let tempReg = 28;      // Use w28 as temp (should be available)
      fprintf(outFile, "  mov w%d, ", tempReg);
      emitValue(state, b.rhs);
      fprintf(outFile, "\n");
      fprintf(outFile, "  %s w%d, w%d, w%d\n", opStr, destReg, destReg, tempReg);
    }
  } else if (rhsImmediateUnsupported) {
    // First operand is register, second is immediate but not supported
    let tempReg = 28;    // Use w28 as temp
    fprintf(outFile, "  mov w%d, ", tempReg);
    emitValue(state, b.rhs);
    fprintf(outFile, "\n");
    fprintf(outFile, "  %s w%d, ", opStr, destReg);
    emitValue(state, b.lhs);
    fprintf(outFile, ", w%d\n", tempReg);
  } else {
    // Normal case: opStr w<dest>, <lhs>, <rhs>
    fprintf(outFile, "  %s w%d, ", opStr, destReg);
    emitValue(state, b.lhs);
    fprintf(outFile, ", ");
    emitValue(state, b.rhs);
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

func emitValue(state: EmitState*, val: Value) {
  switch (val) {
    case Value::IntConstant as i:
      fprintf(outFile, "#%d", i.value);
    case Value::InstrPtr as p:
      let reg = getPhysicalReg(state, p.ptr);
      fprintf(outFile, "w%d", reg);
    case Value::Zero:
      fprintf(outFile, "#0");
    default:
      failEmit("Unhandled value type in emitValue");
  }
}
