import ast;
import ast.print;

import sema.state;
import sema.type;
import sema.expr;
import sema.decl;
import sema.lsp;
import sema.imports;

import debug;

func addTaggedType(state: SemaState*, decl: DeclAST*) {
  switch (decl->kind) {
    case DeclKind::Struct, DeclKind::Enum, DeclKind::Union:
      if (findType(state->types, *getTypeTag(decl->type)) != null) {
        failSemaDecl(state, decl, ": Type redef");
      }

      if (state->semaLspMode) {
        dumpDecl(decl);
      }

      // Add the struct to the types.
      let type = newDeclList(&state->localAlloc, decl);
      type->next = state->types;
      state->types = type;

    default:
      break;
  }
}

func resolveDeclTypeTags(state: SemaState*, decl: DeclAST*) {
  // Skip generic functions until instantiation.
  if (isGeneric(decl)) {
    return;
  }
  resolveTypeTags(state, decl->type);

  switch (decl->kind) {
    case DeclKind::Struct as structKind:
      // Resolve tags in fields.
      for (let field = structKind.fields; field != null; field = field->next) {
        resolveTypeTags(state, field->type);
      }
    case DeclKind::Union as unionKind:
      let maxSize = 0;
      for (let tag = unionKind.subTypes; tag != null; tag = tag->next) {
        // sema the 'tag' which is a struct.
        resolveDeclTypeTags(state, tag->decl);
      }
    case DeclKind::Func as funcKind:
      for (let arg = funcKind.args; arg != null; arg = arg->next) {
        resolveTypeTags(state, arg->type);
      }
    default:
      // Nothing to do for other decl types
      break;
  }
}

func resolveImport(state: SemaState*, decl: DeclAST*) {
  if (state->parent != null) {
    // The parser doesn't accept this.
    unreachable("Import not allowed in local scope");
  }

  let relPath = getImportPath(state, decl);

  // Check if we already import this file.
  for (let cur = state->imports; cur != null; cur = cur->next) {
    if (strcmp(relPath, cur->name) == 0) {
      return;
    }
  }

  // Add to imports
  let imports: ImportList* = alloc(&state->localAlloc, sizeof(struct ImportList));
  imports->name = relPath;
  imports->next = state->imports;
  state->imports = imports;

  let fileBuf = readFile(state->astAlloc, relPath);
  let parseState = ParseState {
    buf = fileBuf,
    fileName = relPath,
    astAlloc = state->astAlloc,
  };

  let fileDecls = parse(&parseState);
  if (fileDecls == null) {
    failSemaDecl(state, decl, "Failed to import file");
  }

  // semaTopLevel will return a combined list of decls from the file and the
  // extraDecls.
  let extras = semaTopLevel(state, fileDecls);
  if (extras != null) {
    state->extraDecls = extras;
  } else {
    failSemaDecl(state, decl, "Error in import");
  }
}

func instantiateGeneric(state: SemaState*, generic: GenericInst*) {
  let newFunc = monomorphize(
      state->astAlloc,
      generic->function,
      generic->typeMap,
      generic->name);

  // printDecl(newFunc);
  resolveDeclTypeTags(state, newFunc);
  semaDecl(state, newFunc);

  newFunc->next = state->extraDecls;
  state->extraDecls = newFunc;
}

func semaTopLevel(state: SemaState*, decl: DeclAST*) -> DeclAST* {
  let fileName = decl->location->fileName;

  // First resolve all imports.
  for (let cur = decl; cur != null; cur = cur->next) {
    if (&cur->kind as DeclKind::Import* != null) {
      resolveImport(state, cur);
    }
  }

  // Add all tagged types to the state, so we can solve them later.
  for (let cur = decl; cur != null; cur = cur->next) {
    addTaggedType(state, cur);
  }

  // Add all functions first, so typeof(func) works.
  // We do this in this stage so declarations can follow uses.
  for (let cur = decl; cur != null; cur = cur->next) {
    if (&cur->kind as DeclKind::Func* != null) {
      addLocalDecl(state, cur);
    }
  }

  // Reolve tagged types to struct / union and lower typeof().
  for (let cur = decl; cur != null; cur = cur->next) {
    resolveDeclTypeTags(state, cur);
  }

  // Do actual type checking and AST transformations.
  for (let cur = decl; cur != null; cur = cur->next) {
    semaDecl(state, cur);
  }

  // sema instantiated generic functions.
  let newInstances = state->genericInstances;
  let prevInstances: GenericInst* = null;
  while (newInstances != prevInstances) {
    for (let cur = newInstances; cur != prevInstances; cur = cur->next) {
      instantiateGeneric(state, cur);
    }
    prevInstances = newInstances;
    newInstances = state->genericInstances;
  }

  if (state->failed) {
    return null;
  }

  // Add extra decls
  if (state->extraDecls != null) {
    let last = state->extraDecls;
    while (last->next != null) {
      last = last->next;
    }
    last->next = decl;
    decl = state->extraDecls;
  } else {
    unreachable("missing _TARGET_ decl");
  }

  // Sema successful, report LSP info in case
  if (state->semaLspMode) {
    fprintf(getStderr(), "%s:%d:%d: OK!\n", fileName, 0, 0);
  }

  return decl;
}

func sema(
    astAlloc: Allocator*,
    target: Target,
    lspMode: bool,
    stdlibPath: i8*,
    decls: DeclAST*
) -> DeclAST* {
  let nullDecl = getNullDecl(astAlloc);
  let targetDecl = getTargetDecl(astAlloc, &target);

  let localAlloc = Allocator {};
  let locals = newDeclList(&localAlloc, nullDecl);
  locals->next = newDeclList(&localAlloc, targetDecl);

  let state = SemaState {
    target = target,
    locals = locals,
    semaLspMode = lspMode,
    extraDecls = targetDecl,
    astAlloc = astAlloc,
    localAlloc = localAlloc,
    stdlibPath = stdlibPath,
  };
  defer freeSemaState(&state);

  state.jmpBuf = newJmpBuf();
  if (setjmp(state.jmpBuf) != 0) {
    return null;
  }

  if (state.stdlibPath == null) {
    findStdlib(&state);
  }

  return semaTopLevel(&state, decls);
}
