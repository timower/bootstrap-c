import ir;
import util;

import ir.type;


// stdout fd number.
let outFile: void* = null;


// Prints the IR in llvm IR format.
func printModule(module: Module*) {
  fprintf(outFile, "target triple = \"%s\"\n\n", module->target.triple);

  for (let type = module->types; type != null; type = type->next) {
    printStruct(type);
  }

  for (let global = module->globals; global != null; global = global->next) {
    printGlobal(global);
  }

  for (let fn = module->functions; fn != null; fn = fn->next) {
    printFunc(fn);
  }
}

func printStruct(type: IRStruct*) {
  fprintf(outFile, "%s = type <{ ", type->name);
  for (let field = type->fields; field != null; field = field->next) {
    fprintf(outFile, "%s", &convertType(field)[0]);
    if (field->next != null) {
      fprintf(outFile, ", ");
    }
  }
  fprintf(outFile, " }>\n");
}


func _getType(value: Value) -> [i8] {
  switch (value) {
    case Value::InstrPtr as p:
      if (p.ptr == null) {
        return "NULL-INSTR!";
      }
      return convertType(p.ptr->type);

    case Value::IntConstant as i:
      return convertType(i.type);

    case Value::StrConstant as s:
      return convertType(s.type);

    case Value::ArrayConstant as a:
      return convertType(a.type);

    case Value::StructConstant as s:
      return convertType(s.type);

    case Value::Argument as a:
      return convertType(a.type);

    case Value::Zero as z:
      return convertType(z.type);

    case Value::FuncPtr as f:
      return "ptr";

    case Value::Sizeof:
      return "i32";

    case Value::GlobalPtr:
      // Globals are always pointers when used.
      return "ptr";

    case Value::AllocaPtr:
      return "ptr";
  }
}


func _getName(value: Value) -> [i8] {
  switch (value) {
    case Value::InstrPtr as p:
      if (p.ptr == null) {
        return "NULL-INSTR!";
      }
      let buf = malloc(32) as i8*;
      let len = sprintf(buf, "%%tmp%d", p.ptr->name);
      return buf[:len];

    case Value::IntConstant as i:
      let buf = malloc(16) as i8*;
      let len = sprintf(buf, "%d", i.value);
      return buf[:len];

    case Value::StrConstant as s:
      let tok = s.value;
      let len = tok.data.len as iptr;
      let data = tok.data;
      let buf = newBuf((len + 16) as iptr);

      let offset = sprintf(&buf[0], "c\"");      // %.*s\\00\"", len, tok.data);

      for (let i: iptr = 0; i < len; i++) {
        let val = data[i];
        if (val == '\\') {
          let c = getEscaped(data[++i]);
          offset += sprintf(&buf[offset], "\\%02x", c);
        } else {
          buf[offset++] = val;
        }
      }
      offset += sprintf(&buf[offset], "\\00\"");
      return buf[:offset];

    case Value::ArrayConstant as a:
      let buf = newBuf(64 * a.values.len as iptr);

      let offset = sprintf(&buf[0], "[ ");

      for (let i = 0; i < a.values.len; i++) {
        let value = a.values[i];
        offset += sprintf(
            &buf[offset],
            "%s %s",
            &_getType(value)[0],
            &_getName(value)[0]);
        if (i != a.values.len - 1) {
          offset += sprintf(&buf[offset], ", ");
        }
      }
      offset += sprintf(&buf[offset], " ]");
      return buf[:offset];

    case Value::StructConstant as s:
      let buf = newBuf(64 * s.values.len as iptr);

      let offset = sprintf(&buf[0], "<{ ");

      // TODO: dedup with array
      for (let i = 0; i < s.values.len; i++) {
        let value = s.values[i];
        offset += sprintf(
            &buf[offset],
            "%s %s",
            &_getType(value)[0],
            &_getName(value)[0]);
        if (i != s.values.len - 1) {
          offset += sprintf(&buf[offset], ", ");
        }
      }
      offset += sprintf(&buf[offset], " }>");
      return buf[:offset];

    case Value::GlobalPtr as g:
      return g.ptr->name;

    case Value::AllocaPtr as a:
      let buf = malloc(32) as i8*;
      let len = sprintf(buf, "%%alloc%d", a.ptr->name);
      return buf[:len];

    case Value::Argument as a:
      let buf = malloc(32) as i8*;
      let len = sprintf(buf, "%%arg%d", a.idx);
      return buf[:len];

    case Value::FuncPtr as f:
      return f.ptr->name;

    case Value::Zero as z:
      if (isAggregate(z.type)) {
        return "zeroinitializer";
      }
      switch (z.type->kind) {
        case TypeKind::Array:
          return "zeroinitializer";
        case TypeKind::Int:
          return "0";
        case TypeKind::Bool:
          return "false";
        default:
          return "null";
      }

    case Value::Sizeof as s:
      let buf = malloc(128) as i8*;
      let len = sprintf(
          buf,
          "ptrtoint (ptr getelementptr (%s, ptr null, i32 1) to i32)",
          &convertType(s.type)[0]);
      return buf[:len];
  }
}

func printGlobal(global: Global*) {
  let declSpec =
      global->type->isConst
       ? "constant"[:]
       : "global"[:];

  if (global->isExtern) {
    fprintf(
        outFile,
        "%s = external %s %s\n",
        &global->name[0],
        &declSpec[0],
        &convertType(global->type)[0]);
  } else {
    fprintf(
        outFile,
        "%s = %s %s %s\n",
        &global->name[0],
        &declSpec[0],
        &_getType(global->init)[0],
        &_getName(global->init)[0]);
  }
}

func printFunc(fn: Function*) {
  let fnType = fn->type->kind as TypeKind::Func*;
  let isEmpty = fn->begin == null;
  let defOrDecl = isEmpty ? "declare"[:] : "define"[:];

  fprintf(outFile, "%s %s %s(", &defOrDecl[0], &convertType(fnType->result)[0], &fn->name[0]);

  let idx = 0;
  for (let arg = fnType->args; arg != null; arg = arg->next, idx++) {
    fprintf(outFile, "%s %%arg%d", &convertType(arg)[0], idx);
    if (arg->next != null || fnType->isVarargs) {
      fprintf(outFile, ", ");
    }
  }

  if (fnType->isVarargs) {
    fprintf(outFile, "...");
  }

  fprintf(outFile, ")");

  if (isEmpty) {
    fprintf(outFile, "\n");
    return;
  }
  fprintf(outFile, " {\n");

  // print alloca instructions
  for (let alloc = fn->allocs; alloc != null; alloc = alloc->next) {
    let val = Value::AllocaPtr {
      ptr = alloc,
    };
    fprintf(
        outFile,
        "  %s = alloca %s\n",
        &_getName(val)[0],
        &convertType(alloc->type)[0]);
  }
  if (fn->allocs != null) {
    fprintf(outFile, "  br label %%%s\n", getBBName(fn->begin));
  }

  // print the entry block label
  for (let bb = fn->begin; bb != null; bb = bb->next) {
    printBB(bb);
  }

  fprintf(outFile, "}\n\n");
}

func getBBName(bb: BasicBlock*) -> i8* {
  let buf = malloc(32);
  sprintf(buf, "%s.%d", bb->label, bb->name);
  return buf;
}

func printBB(bb: BasicBlock*) {
  fprintf(outFile, "%s:\n", getBBName(bb));
  for (let instr = bb->begin; instr != null; instr = instr->next) {
    printInstr(instr);
  }
}

func printInstr(instr: Instruction*) {
  fprintf(outFile, "  ");

  // Non void instructions have a name.
  if (hasResult(instr)) {
    fprintf(outFile, "%%tmp%d = ", instr->name);
  }

  switch (instr->kind) {
    case InstrKind::StructGEP as g:
      fprintf(
          outFile,
          "getelementptr inbounds %s, ptr %s, i32 0, i32 %d",
          &convertType(g.type)[0],
          &_getName(g.ptr)[0],
          g.field);

    case InstrKind::ArrayGEP as g:
      fprintf(
          outFile,
          "getelementptr inbounds %s, ptr %s, %s %s",
          &convertType(g.type)[0],
          &_getName(g.ptr)[0],
          &_getType(g.idx)[0],
          &_getName(g.idx)[0]);

    case InstrKind::Binary as b:
      let binStr: [i8] = nullBuf();
      switch (b.op) {
        case BinaryOp::Add:
          binStr = "add";
        case BinaryOp::Sub:
          binStr = "sub";
        case BinaryOp::Mul:
          binStr = "mul";
        case BinaryOp::SDiv:
          binStr = "sdiv";
        case BinaryOp::SRem:
          binStr = "srem";
        case BinaryOp::Shl:
          binStr = "shl";
        case BinaryOp::AShr:
          binStr = "ashr";
        case BinaryOp::LShr:
          binStr = "lshr";
        case BinaryOp::And:
          binStr = "and";
        case BinaryOp::Xor:
          binStr = "xor";
        case BinaryOp::Or:
          binStr = "or";
      }
      fprintf(
          outFile,
          "%s %s %s, %s",
          &binStr[0],
          &_getType(b.lhs)[0],
          &_getName(b.lhs)[0],
          &_getName(b.rhs)[0]);

    case InstrKind::Cmp as c:
      let condStr: [i8] = nullBuf();
      switch (c.op) {
        case CmpOp::Eq:
          condStr = "eq";
        case CmpOp::Ne:
          condStr = "ne";
        case CmpOp::Slt:
          condStr = "slt";
        case CmpOp::Sle:
          condStr = "sle";
        case CmpOp::Sgt:
          condStr = "sgt";
        case CmpOp::Sge:
          condStr = "sge";
      }
      fprintf(
          outFile,
          "icmp %s %s %s, %s",
          &condStr[0],
          &_getType(c.lhs)[0],
          &_getName(c.lhs)[0],
          &_getName(c.rhs)[0]);

    case InstrKind::Cast as c:
      let castStr: [i8] = nullBuf();
      switch (c.kind) {
        case CastKind::Zext:
          castStr = "zext";
        case CastKind::Sext:
          castStr = "sext";
        case CastKind::Trunc:
          castStr = "trunc";
        case CastKind::PtrToInt:
          castStr = "ptrtoint";
        default:
          fprintf(outFile, "Error, cast print!");
          exit(1);
      }
      fprintf(
          outFile,
          "%s %s %s to %s",
          &castStr[0],
          &_getType(c.val)[0],
          &_getName(c.val)[0],
          &convertType(instr->type)[0]);

    case InstrKind::Call as c:
      fprintf(outFile, "call %s %s(", &convertType(c.fnType)[0], &_getName(c.fn)[0]);
      for (let i = 0; i < c.args.len; i++) {
        fprintf(outFile, "%s %s", &_getType(c.args[i])[0], &_getName(c.args[i])[0]);
        if (i != c.args.len - 1) {
          fprintf(outFile, ", ");
        }
      }
      fprintf(outFile, ")");

    case InstrKind::Select as s:
      fprintf(
          outFile,
          "select i1 %s, %s %s, %s %s",
          &_getName(s.cond)[0],
          &_getType(s.trueVal)[0],
          &_getName(s.trueVal)[0],
          &_getType(s.falseVal)[0],
          &_getName(s.falseVal)[0]);

    case InstrKind::Switch as s:
      fprintf(
          outFile,
          "switch %s %s, label %%%s [\n",
          &_getType(s.cond)[0],
          &_getName(s.cond)[0],
          getBBName(s.defaultBB));

      // Print all cases
      for (let cse = s.cases; cse != null; cse = cse->next) {
        fprintf(
            outFile,
            "    %s %s, label %%%s\n",
            &_getType(cse->val)[0],
            &_getName(cse->val)[0],
            getBBName(cse->bb));
      }
      fprintf(outFile, "  ]");

    case InstrKind::Phi as p:
      fprintf(
          outFile,
          "phi %s [ %s, %%%s ], [ %s, %%%s ]",
          &convertType(instr->type)[0],
          &_getName(p.trueVal)[0],
          getBBName(p.trueBB),
          &_getName(p.falseVal)[0],
          getBBName(p.falseBB));

    case InstrKind::Branch as b:
      fprintf(outFile, "br label %%%s", getBBName(b.bb));

    case InstrKind::CondBranch as b:
      fprintf(
          outFile,
          "br %s %s, label %%%s, label %%%s",
          &_getType(b.cond)[0],
          &_getName(b.cond)[0],
          getBBName(b.trueBB),
          getBBName(b.falseBB));

    case InstrKind::Store as s:
      fprintf(
          outFile,
          "store %s %s, ptr %s",
          &_getType(s.val)[0],
          &_getName(s.val)[0],
          &_getName(s.ptr)[0]);

    case InstrKind::Load as l:
      fprintf(
          outFile,
          "load %s, ptr %s",
          &convertType(instr->type)[0],
          &_getName(l.ptr)[0]);

    case InstrKind::Return as r:
      fprintf(outFile, "ret %s %s", &_getType(r.val)[0], &_getName(r.val)[0]);
    case InstrKind::ReturnVoid:
      fprintf(outFile, "ret void");
  }

  fprintf(outFile, "\n");
}
