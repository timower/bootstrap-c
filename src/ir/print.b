import ir;
import util;

import ir.type;


// stdout fd number.
let outFile: void* = null;


// Prints the IR in llvm IR format.
func printModule(module: Module*) {
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
    fprintf(outFile, "%s", convertType(field));
    if (field->next != null) {
      fprintf(outFile, ", ");
    }
  }
  fprintf(outFile, " }>\n");
}


func getType(value: Value) -> i8* {
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

    case Value::Argument as a:
      return convertType(a.type);

    case Value::Zero as z:
      return convertType(z.type);

    case Value::FuncPtr as f:
      return convertType(f.ptr->type);

    case Value::Sizeof:
      return "i32";

    case Value::GlobalPtr:
      // Globals are always pointers when used.
      return "ptr";

    case Value::AllocaPtr:
      return "ptr";
  }
}

func getName(value: Value) -> i8* {
  // let kindPtr = &value as void*;
  // let kindPtrT = kindPtr as i32*;
  // let buf = malloc(32);
  // sprintf(buf, "kind-%d", *kindPtrT);
  // return buf;
  switch (value) {
    case Value::InstrPtr as p:
      if (p.ptr == null) {
        return "NULL-INSTR!";
      }
      let buf = malloc(32);
      sprintf(buf, "%%tmp%d", p.ptr->name);
      return buf;

    case Value::IntConstant as i:
      let buf: i8* = malloc(16);
      sprintf(buf, "%d", i.value);
      return buf;

    case Value::StrConstant as s:
      let tok = s.value;
      let len = tok.end - tok.data;
      let val: i8* = malloc((len + 16) as u64);

      let cur = val;
      cur += sprintf(val, "c\"");      // %.*s\\00\"", len, tok.data);

      for (let i: i64 = 0; i < len; i++) {
        let val = *(tok.data + i);
        if (val == 92) {
          let c = getEscaped(*(tok.data + ++i));
          cur += sprintf(cur, "\\%02x", c);
        } else {
          *cur++ = val;
        }
      }
      cur += sprintf(cur, "\\00\"");
      return val;

    case Value::ArrayConstant as a:
      let buf: i8* = malloc((64 * a.size as i64) as u64);
      let res = buf;

      buf += sprintf(buf, "[ ");

      for (let i = 0; i < a.size; i++) {
        let value = *(a.values + i);
        buf += sprintf(buf, "%s %s", getType(value), getName(value));
        if (i != a.size - 1) {
          buf += sprintf(buf, ", ");
        }
      }
      buf += sprintf(buf, " ]");
      return res;

    case Value::GlobalPtr as g:
      return g.ptr->name;

    case Value::AllocaPtr as a:
      let buf = malloc(32);
      sprintf(buf, "%%alloc%d", a.ptr->name);
      return buf;

    case Value::Argument as a:
      let buf = malloc(32);
      sprintf(buf, "%%arg%d", a.idx);
      return buf;

    case Value::FuncPtr as f:
      return f.ptr->name;

    case Value::Zero as z:
      if (isAggregate(z.type)) {
        return "zeroinitializer";
      }
      if (z.type->kind as TypeKind::Int* != null) {
        return "0";
      }
      if (z.type->kind as TypeKind::Bool* != null) {
        return "false";
      }
      return "null";

    case Value::Sizeof as s:
      let buf = malloc(128);
      sprintf(
          buf,
          "ptrtoint (ptr getelementptr (%s, ptr null, i32 1) to i32)",
          convertType(s.type));
      return buf;
  }
}

func printGlobal(global: Global*) {
  let declSpec =
      global->type->isConst
       ? "constant" as i8*
       : "global" as i8*;

  if (global->isExtern) {
    fprintf(
        outFile,
        "%s = external %s %s\n",
        global->name,
        declSpec,
        convertType(global->type));
  } else {
    fprintf(
        outFile,
        "%s = %s %s %s\n",
        global->name,
        declSpec,
        getType(global->init),
        getName(global->init));
  }
}

func printFunc(fn: Function*) {
  let fnType = fn->type->kind as TypeKind::Func*;
  let isEmpty = fn->begin == null;
  let defOrDecl = isEmpty ? "declare" as i8* : "define" as i8*;

  fprintf(outFile, "%s %s %s(", defOrDecl, convertType(fnType->result), fn->name);

  let idx = 0;
  for (let arg = fnType->args; arg != null; arg = arg->next, idx++) {
    fprintf(outFile, "%s %%arg%d", convertType(arg), idx);
    if (arg->next != null) {
      fprintf(outFile, ", ");
    }
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
    fprintf(outFile, "  %s = alloca %s\n", getName(val), convertType(alloc->type));
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
          convertType(g.type),
          getName(g.ptr),
          g.field);

    case InstrKind::ArrayGEP as g:
      fprintf(
          outFile,
          "getelementptr inbounds %s, ptr %s, %s %s",
          convertType(g.type),
          getName(g.ptr),
          getType(g.idx),
          getName(g.idx));

    case InstrKind::Binary as b:
      let binStr: i8* = null;
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
          binStr,
          getType(b.lhs),
          getName(b.lhs),
          getName(b.rhs));

    case InstrKind::Cmp as c:
      let condStr: i8* = null;
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
          condStr,
          getType(c.lhs),
          getName(c.lhs),
          getName(c.rhs));

    case InstrKind::Cast as c:
      let castStr: i8* = null;
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
          castStr,
          getType(c.val),
          getName(c.val),
          convertType(instr->type));

    case InstrKind::Call as c:
      fprintf(outFile, "call %s %s(", getType(c.fn), getName(c.fn));
      for (let i = 0; i < c.numArgs; i++) {
        fprintf(outFile, "%s %s", getType(*(c.args + i)), getName(*(c.args + i)));
        if (i != c.numArgs - 1) {
          fprintf(outFile, ", ");
        }
      }
      fprintf(outFile, ")");

    case InstrKind::Select as s:
      fprintf(
          outFile,
          "select i1 %s, %s %s, %s %s",
          getName(s.cond),
          getType(s.trueVal),
          getName(s.trueVal),
          getType(s.falseVal),
          getName(s.falseVal));

    case InstrKind::Switch as s:
      fprintf(
          outFile,
          "switch %s %s, label %%%s [\n",
          getType(s.cond),
          getName(s.cond),
          getBBName(s.defaultBB));

      // Print all cases
      for (let cse = s.cases; cse != null; cse = cse->next) {
        fprintf(
            outFile,
            "    %s %s, label %%%s\n",
            getType(cse->val),
            getName(cse->val),
            getBBName(cse->bb));
      }
      fprintf(outFile, "  ]");

    case InstrKind::Phi as p:
      fprintf(
          outFile,
          "phi %s [ %s, %%%s ], [ %s, %%%s ]",
          convertType(instr->type),
          getName(p.trueVal),
          getBBName(p.trueBB),
          getName(p.falseVal),
          getBBName(p.falseBB));

    case InstrKind::Branch as b:
      fprintf(outFile, "br label %%%s", getBBName(b.bb));

    case InstrKind::CondBranch as b:
      fprintf(
          outFile,
          "br %s %s, label %%%s, label %%%s",
          getType(b.cond),
          getName(b.cond),
          getBBName(b.trueBB),
          getBBName(b.falseBB));

    case InstrKind::Store as s:
      fprintf(
          outFile,
          "store %s %s, ptr %s",
          getType(s.val),
          getName(s.val),
          getName(s.ptr));

    case InstrKind::Load as l:
      fprintf(
          outFile,
          "load %s, ptr %s",
          convertType(instr->type),
          getName(l.ptr));

    case InstrKind::Return as r:
      fprintf(outFile, "ret %s %s", getType(r.val), getName(r.val));
    case InstrKind::ReturnVoid:
      fprintf(outFile, "ret void");
  }

  fprintf(outFile, "\n");
}
