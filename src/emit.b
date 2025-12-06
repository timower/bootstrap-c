import libc;
import target;

import ir;
import ir.print;

struct EmitState {
  // Register allocation state
  usedRegs: bool[32];  // Track which registers are in use (w0-w31)

  // Virtual register to physical register mapping
  instrToReg: i32*;  // Map instruction names to physical registers
  instrToRegSize: i32;  // Size of the mapping array
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

func setInstrReg(instrName: i32, reg: i32, state: EmitState*) {
  if (instrName >= state->instrToRegSize) {
    failEmit("Instruction name out of bounds in register mapping");
  }
  *(state->instrToReg + instrName) = reg;
}

func getInstrReg(instrName: i32, state: EmitState*) -> i32 {
  if (instrName >= state->instrToRegSize) {
    failEmit("Instruction name out of bounds in register mapping");
  }
  return *(state->instrToReg + instrName);
}

func failEmit(msg: i8*) {
  printf("Emit error: %s\n", msg);
  exit(1);
}

func freeRegister(instrName: i32, state: EmitState*) {
  // When we reach a definition walking backwards, we can free its register
  // for reuse by earlier instructions (later in execution)
  if (instrName < state->instrToRegSize) {
    let reg = getInstrReg(instrName, state);
    if (reg != -1 && reg > 0) {
      // Don't free w0 (reserved for returns)
      state->usedRegs[reg] = false;
    }
  }
}

func getPhysicalReg(instrName: i32, state: EmitState*) -> i32 {
  // Get the allocated physical register for an instruction
  if (instrName >= state->instrToRegSize) {
    failEmit("Instruction name out of bounds in register mapping");
  }

  let reg = getInstrReg(instrName, state);
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
  return -1;  // No more registers available
}


func allocateRegistersBackwards(fn: Function*, state: EmitState*) {
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
  let size = (state->instrToRegSize * 4) as iptr;
  state->instrToReg = malloc(size as uptr) as i32*;

  // Initialize all mappings to -1 (unassigned)
  for (let i = 0; i < state->instrToRegSize; i = i + 1) {
    setInstrReg(i, -1, state);
  }

  // Walk backwards through basic blocks
  for (let bb = fn->end; bb != null; bb = bb->prev) {
    // Walk backwards through instructions in this block
    for (let instr = bb->end; instr != null; instr = instr->prev) {
      processInstructionBackwards(instr, state);
    }
  }
}

func processInstructionBackwards(instr: Instruction*, state: EmitState*) {
  // Process uses first - these will allocate registers via markAsLive
  processUsesBackwards(instr, state);

  // Then process definition - this is where the register lifetime starts (walking backwards)
  // The register should already be allocated by markAsLive, just free it for reuse
  if (hasResult(instr)) {
    freeRegister(instr->name, state);
  }
}

func processUsesBackwards(instr: Instruction*, state: EmitState*) {
  switch (instr->kind) {
    case InstrKind::Binary as b:
      markAsLive(b.lhs, state);
      markAsLive(b.rhs, state);
    case InstrKind::Return as r:
      markAsLive(r.val, state);
    default:
      failEmit("Unhandled instruction kind in processUsesBackwards");
  }
}

func markAsLive(val: Value, state: EmitState*) {
  switch (val) {
    case Value::InstrPtr as p:
      // Walking backwards: this use means we need to allocate a register
      // if one hasn't been allocated yet
      if ((p.ptr)->name < state->instrToRegSize) {
        let reg = getInstrReg((p.ptr)->name, state);
        if (reg == -1) {
          // First time seeing this value (walking backwards), allocate register
          reg = allocateNextRegister(state);
          if (reg == -1) {
            failEmit("Function needs too many registers for allocation");
          }
          setInstrReg((p.ptr)->name, reg, state);
        }

        // Mark register as used so it won't be reused
        state->usedRegs[reg] = true;
      }

    default:
      // Constants, globals, etc. don't need physical registers
      break;
  }
}

func emitAsm(module: Module*, target: Target) {
  let useUnderscore = target.platform == Platform::Darwin;
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
  allocateRegistersBackwards(fn, &state);

  for (let bb = fn->begin; bb != null; bb = bb->next) {
    emitBasicBlock(bb, &state);
  }
  // TODO: Clean up instrToReg allocation when free() is available
}

func emitBasicBlock(bb: BasicBlock*, state: EmitState*) {
  for (let instr = bb->begin; instr != null; instr = instr->next) {
    emitInstruction(instr, state);
  }
}

func emitInstruction(instr: Instruction*, state: EmitState*) {
  switch (instr->kind) {
    case InstrKind::Binary as b:
      emitBinaryOp(instr, b, state);
    case InstrKind::Return as r:
      fprintf(outFile, "  mov w0, ");
      emitValue(r.val, state);
      fprintf(outFile, "\n");
      fprintf(outFile, "  ret\n");
    case InstrKind::ReturnVoid:
      fprintf(outFile, "  ret\n");

    default:
      // TODO: handle other instructions
      failEmit("Unhandled instruction kind in emitInstruction");
  }
}

func emitBinaryOp(instr: Instruction*, b: InstrKind::Binary, state: EmitState*) {
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
  let destReg = getPhysicalReg(instr->name, state);

  // For ARM64, we need to handle immediate operands carefully
  let bothImmediate = isImmediate(b.lhs) && isImmediate(b.rhs);
  let rhsImmediateUnsupported = isImmediate(b.rhs) && !supportsImmediate;

  if (bothImmediate) {
    // Load first operand into destination register, keep second as immediate if supported
    fprintf(outFile, "  mov w%d, ", destReg);
    emitValue(b.lhs, state);
    fprintf(outFile, "\n");

    if (supportsImmediate) {
      // Can use immediate for second operand
      fprintf(outFile, "  %s w%d, w%d, ", opStr, destReg, destReg);
      emitValue(b.rhs, state);
      fprintf(outFile, "\n");
    } else {
      // Need to load second operand into a temp register
      // For now, use a hardcoded temp register - this should be improved
      let tempReg = 28;      // Use w28 as temp (should be available)
      fprintf(outFile, "  mov w%d, ", tempReg);
      emitValue(b.rhs, state);
      fprintf(outFile, "\n");
      fprintf(outFile, "  %s w%d, w%d, w%d\n", opStr, destReg, destReg, tempReg);
    }
  } else if (rhsImmediateUnsupported) {
    // First operand is register, second is immediate but not supported
    let tempReg = 28;    // Use w28 as temp
    fprintf(outFile, "  mov w%d, ", tempReg);
    emitValue(b.rhs, state);
    fprintf(outFile, "\n");
    fprintf(outFile, "  %s w%d, ", opStr, destReg);
    emitValue(b.lhs, state);
    fprintf(outFile, ", w%d\n", tempReg);
  } else {
    // Normal case: opStr w<dest>, <lhs>, <rhs>
    fprintf(outFile, "  %s w%d, ", opStr, destReg);
    emitValue(b.lhs, state);
    fprintf(outFile, ", ");
    emitValue(b.rhs, state);
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

func emitValue(val: Value, state: EmitState*) {
  switch (val) {
    case Value::IntConstant as i:
      fprintf(outFile, "#%d", i.value);
    case Value::InstrPtr as p:
      let reg = getPhysicalReg((p.ptr)->name, state);
      fprintf(outFile, "w%d", reg);
    case Value::Zero:
      fprintf(outFile, "#0");
    default:
      failEmit("Unhandled value type in emitValue");
  }
}
