import ir;
import util;

import ir.type;


// stdout fd number.
let outFile: void* = null;


// Prints the IR in llvm IR format.
func printModule(module: Module*) {
  let allocator = Allocator {};
  defer freeAll(&allocator);
  fprintf(outFile, "target triple = \"%s\"\n\n", module->target.triple);

  for (let type = module->types; type != null; type = type->next) {
    printStruct(&allocator, type);
  }

  for (let global = module->globals; global != null; global = global->next) {
    printGlobal(&allocator, global);
  }

  for (let fn = module->functions; fn != null; fn = fn->next) {
    printFunc(&allocator, fn);
  }
}

func printStruct(a: Allocator*, type: IRStruct*) {
  fprintf(outFile, "%s = type <{ ", type->name);
  for (let field = type->fields; field != null; field = field->next) {
    fprintf(outFile, "%s", &convertType(a, field)[0]);
    if (field->next != null) {
      fprintf(outFile, ", ");
    }
  }
  fprintf(outFile, " }>\n");
}


func _getType(a: Allocator*, value: Value) -> [i8] {
  switch (value) {
    case Value::InstrPtr as p:
      if (p.ptr == null) {
        unreachable("NULL-INSTR!");
      }
      return convertType(a, p.ptr->type);

    case Value::IntConstant as i:
      return convertType(a, i.type);

    case Value::StrConstant as s:
      return convertType(a, s.type);

    case Value::ArrayConstant as arr:
      return convertType(a, arr.type);

    case Value::StructConstant as s:
      return convertType(a, s.type);

    case Value::Argument as arg:
      return convertType(a, arg.type);

    case Value::Zero as z:
      return convertType(a, z.type);

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


func _getName(a: Allocator*, value: Value) -> [i8] {
  switch (value) {
    case Value::InstrPtr as p:
      if (p.ptr == null) {
        unreachable("NULL-INSTR!");
      }
      let buf = alloc(a, 32) as i8*;
      let len = sprintf(buf, "%%tmp%d", p.ptr->name);
      return buf[:len];

    case Value::IntConstant as i:
      let buf = alloc(a, 16) as i8*;
      let len = sprintf(buf, "%d", i.value);
      return buf[:len];

    case Value::StrConstant as s:
      let tok = s.value;
      let len = tok.data.len as iptr;
      let data = tok.data;
      let buf = newBuf(a, (len * 2) + 16 as iptr);

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

    case Value::ArrayConstant as arr:
      let buf = newBuf(a, 64 * arr.values.len as iptr);

      let offset = sprintf(&buf[0], "[ ");

      for (let i = 0; i < arr.values.len; i++) {
        let value = arr.values[i];
        offset += sprintf(
            &buf[offset],
            "%s %s",
            &_getType(a, value)[0],
            &_getName(a, value)[0]);
        if (i != arr.values.len - 1) {
          offset += sprintf(&buf[offset], ", ");
        }
      }
      offset += sprintf(&buf[offset], " ]");
      return buf[:offset];

    case Value::StructConstant as s:
      let buf = newBuf(a, 64 * s.values.len as iptr);

      let offset = sprintf(&buf[0], "<{ ");

      // TODO: dedup with array
      for (let i = 0; i < s.values.len; i++) {
        let value = s.values[i];
        offset += sprintf(
            &buf[offset],
            "%s %s",
            &_getType(a, value)[0],
            &_getName(a, value)[0]);
        if (i != s.values.len - 1) {
          offset += sprintf(&buf[offset], ", ");
        }
      }
      offset += sprintf(&buf[offset], " }>");
      return buf[:offset];

    case Value::GlobalPtr as g:
      return g.ptr->name;

    case Value::AllocaPtr as aptr:
      let nameLen = aptr.ptr->dbgName.len;
      let buf = alloc(a, 32 + nameLen) as i8*;
      let len = sprintf(
          buf,
          "%%%.*s.%d",
          nameLen,
          &aptr.ptr->dbgName[0],
          aptr.ptr->name);
      return buf[:len];

    case Value::Argument as arg:
      let buf = alloc(a, 32) as i8*;
      let len = sprintf(buf, "%%arg%d", arg.idx);
      return buf[:len];

    case Value::FuncPtr as f:
      return f.ptr->name;

    case Value::Zero as z:
      if (isAggregate(z.type)) {
        return "zeroinitializer";
      }
      switch (z.type->kind) {
        case TypeKind::Int, TypeKind::Enum:
          return "0";
        case TypeKind::Bool:
          return "false";
        default:
          return "null";
      }

    case Value::Sizeof as s:
      let buf = alloc(a, 128) as i8*;
      let len = sprintf(
          buf,
          "ptrtoint (ptr getelementptr (%s, ptr null, i32 1) to i32)",
          &convertType(a, s.type)[0]);
      return buf[:len];
  }
}

func printGlobal(a: Allocator*, global: Global*) {
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
        &convertType(a, global->type)[0]);
  } else {
    fprintf(
        outFile,
        "%s = %s %s %s\n",
        &global->name[0],
        &declSpec[0],
        &_getType(a, global->init)[0],
        &_getName(a, global->init)[0]);
  }
}

func printFunc(a: Allocator*, fn: Function*) {
  let fnType = fn->type->kind as TypeKind::Func*;
  let isEmpty = fn->begin == null;
  let defOrDecl = isEmpty ? "declare"[:] : "define"[:];

  fprintf(outFile, "%s %s %s(", &defOrDecl[0], &convertType(a, fnType->result)[0], &fn->name[0]);

  let idx = 0;
  for (let arg = fnType->args; arg != null; arg = arg->next, idx++) {
    fprintf(outFile, "%s %%arg%d", &convertType(a, arg)[0], idx);
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
        &_getName(a, val)[0],
        &convertType(a, alloc->type)[0]);
  }
  if (fn->allocs != null) {
    fprintf(outFile, "  br label %%%s\n", getBBName(a, fn->begin));
  }

  // print the entry block label
  for (let bb = fn->begin; bb != null; bb = bb->next) {
    printBB(a, bb);
  }

  fprintf(outFile, "}\n\n");
}

func getBBName(a: Allocator*, bb: BasicBlock*) -> i8* {
  let name = bb->location->fileName[:strlen(bb->location->fileName)];

  let ptr = alloc(a, 32 + name.len) as i8*;
  let buf = ptr[:32 + name.len];

  let offset = sprintf(ptr, "%s.%d.", bb->label, bb->name);
  for (let i = 0; i < name.len; i++) {
    if (name[i] == '/') {
      buf[offset] = '_';
    } else {
      buf[offset] = name[i];
    }
    offset++;
  }
  sprintf(&buf[offset], ".%d", bb->location->line);

  return ptr;
}

func printBB(a: Allocator*, bb: BasicBlock*) {
  fprintf(outFile, "%s:\n", getBBName(a, bb));
  for (let instr = bb->begin; instr != null; instr = instr->next) {
    printInstr(a, instr);
  }
}

func printInstr(a: Allocator*, instr: Instruction*) {
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
          &convertType(a, g.type)[0],
          &_getName(a, g.ptr)[0],
          g.field);

    case InstrKind::ArrayGEP as g:
      fprintf(
          outFile,
          "getelementptr inbounds %s, ptr %s, %s %s",
          &convertType(a, g.type)[0],
          &_getName(a, g.ptr)[0],
          &_getType(a, g.idx)[0],
          &_getName(a, g.idx)[0]);

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
          &_getType(a, b.lhs)[0],
          &_getName(a, b.lhs)[0],
          &_getName(a, b.rhs)[0]);

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
          &_getType(a, c.lhs)[0],
          &_getName(a, c.lhs)[0],
          &_getName(a, c.rhs)[0]);

    case InstrKind::Cast as c:
      let castStr: [i8] = nullBuf();
      switch (c.kind) {
        case CastKind::Zext:
          castStr = "zext";
        case CastKind::Sext:
          castStr = "sext";
        case CastKind::Trunc:
          castStr = "trunc";
        default:
          unreachable("Error, cast print!");
      }
      fprintf(
          outFile,
          "%s %s %s to %s",
          &castStr[0],
          &_getType(a, c.val)[0],
          &_getName(a, c.val)[0],
          &convertType(a, instr->type)[0]);

    case InstrKind::Call as c:
      fprintf(outFile, "call %s %s(", &convertType(a, c.fnType)[0], &_getName(a, c.fn)[0]);
      for (let i = 0; i < c.args.len; i++) {
        fprintf(outFile, "%s %s", &_getType(a, c.args[i])[0], &_getName(a, c.args[i])[0]);
        if (i != c.args.len - 1) {
          fprintf(outFile, ", ");
        }
      }
      fprintf(outFile, ")");

    case InstrKind::Select as s:
      fprintf(
          outFile,
          "select i1 %s, %s %s, %s %s",
          &_getName(a, s.cond)[0],
          &_getType(a, s.trueVal)[0],
          &_getName(a, s.trueVal)[0],
          &_getType(a, s.falseVal)[0],
          &_getName(a, s.falseVal)[0]);

    case InstrKind::Switch as s:
      fprintf(
          outFile,
          "switch %s %s, label %%%s [\n",
          &_getType(a, s.cond)[0],
          &_getName(a, s.cond)[0],
          getBBName(a, s.defaultBB));

      // Print all cases
      for (let cse = s.cases; cse != null; cse = cse->next) {
        fprintf(
            outFile,
            "    %s %s, label %%%s\n",
            &_getType(a, cse->val)[0],
            &_getName(a, cse->val)[0],
            getBBName(a, cse->bb));
      }
      fprintf(outFile, "  ]");

    case InstrKind::Phi as p:
      fprintf(
          outFile,
          "phi %s [ %s, %%%s ], [ %s, %%%s ]",
          &convertType(a, instr->type)[0],
          &_getName(a, p.trueVal)[0],
          getBBName(a, p.trueBB),
          &_getName(a, p.falseVal)[0],
          getBBName(a, p.falseBB));

    case InstrKind::Branch as b:
      fprintf(outFile, "br label %%%s", getBBName(a, b.bb));

    case InstrKind::CondBranch as b:
      fprintf(
          outFile,
          "br %s %s, label %%%s, label %%%s",
          &_getType(a, b.cond)[0],
          &_getName(a, b.cond)[0],
          getBBName(a, b.trueBB),
          getBBName(a, b.falseBB));

    case InstrKind::Store as s:
      fprintf(
          outFile,
          "store %s %s, ptr %s",
          &_getType(a, s.val)[0],
          &_getName(a, s.val)[0],
          &_getName(a, s.ptr)[0]);

    case InstrKind::Load as l:
      fprintf(
          outFile,
          "load %s, ptr %s",
          &convertType(a, instr->type)[0],
          &_getName(a, l.ptr)[0]);

    case InstrKind::Return as r:
      fprintf(outFile, "ret %s %s", &_getType(a, r.val)[0], &_getName(a, r.val)[0]);
    case InstrKind::ReturnVoid:
      fprintf(outFile, "ret void");
  }

  fprintf(outFile, "\n");
}
