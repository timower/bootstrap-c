import libc;
import util;
import ast;
import ast.print;

import emit.state;
import emit.type;
import emit.utils;
import emit.expr;
import emit.stmt;
import emit.decl;


func emitTopLevel(decl: DeclAST*) {
  let rootCounter = 0;
  let state = EmitState {
    tmpCounter = &rootCounter,
  };

  for (let cur = decl; cur != null; cur = cur->next) {
    addGlobalDecl(&state, cur);
  }

  for (let cur = decl; cur != null; cur = cur->next) {
    emitGlobalDecl(&state, cur);
  }
}
