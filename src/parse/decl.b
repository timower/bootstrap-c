import ast;

import state;
import token;
import expr;
import stmt;

func optionalSemicolon(state: ParseState*) {
  if (match(state, TokenKind::SEMICOLON)) {
    getNextToken(state);
  }
}


// Add any comments in state that are on the same line as decl to decl.
func addTrailingCommentsDecl(state: ParseState*, decl: DeclAST*) {
  let comments = getLineComments(state, decl->endLocation->line);
  if (comments == null) {
    return;
  }

  decl->comments = appendComments(decl->comments, comments);
}


// type_name_pair := identifier ':' type
func parseNameTypePair(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::Var {});

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  expect(state, TokenKind::COLON);
  getNextToken(state);

  decl->type = parseType(state);
  decl->endLocation = state->curToken.location;

  return decl;
}

func parseSubStruct(state: ParseState*, decl: DeclAST*) {
  // (non)optional tag
  expect(state, TokenKind::IDENTIFIER);

  decl->type = newType(TypeKind::Struct {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);  // eat {

  decl->kind = DeclKind::Struct {};

  // parse the fields
  let fields = decl;
  let firstField: DeclAST* = null;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    let field = parseNameTypePair(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);    // eat ;

    addTrailingCommentsDecl(state, field);

    if (firstField == null) {
      firstField = field;
      fields = field;
    } else {
      fields->next = field;
      fields = field;
    }
  }
  decl->endLocation = getNextToken(state).location;  // eat }

  (&decl->kind as DeclKind::Struct*)->fields = firstField;
}


// struct := 'struct' identifier '{' decl* '}'
func parseStruct(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::Struct {});
  getNextToken(state);  // eat struct
  parseSubStruct(state, decl);
  return decl;
}


// enum := 'enum' identifier '{' identifier  (',' identifier )* ','? '}'
func parseEnum(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::Enum {});
  getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);

  decl->type = newType(TypeKind::Enum {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  // parse constants
  let firstField: DeclAST* = null;
  let fields: DeclAST* = null;
  let idx = 0;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    expect(state, TokenKind::IDENTIFIER);

    let field = newLocDecl(state, DeclKind::EnumField {});
    field->type = getInt32();

    field->name = getNextToken(state);
    (&field->kind as DeclKind::EnumField*)->enumValue = idx++;

    if (firstField == null) {
      firstField = field;
      fields = field;
    } else {
      fields->next = field;
      fields = field;
    }

    field->endLocation = state->curToken.location;

    if (match(state, TokenKind::CLOSE_BRACE)) {
      addTrailingCommentsDecl(state, field);
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);

    addTrailingCommentsDecl(state, field);
  }
  decl->endLocation = getNextToken(state).location;  // eat }

  (&decl->kind as DeclKind::Enum*)->fields = firstField;
  return decl;
}

func parseUnion(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::Union {});
  getNextToken(state);  // eat 'union'

  expect(state, TokenKind::IDENTIFIER);
  decl->type = newType(TypeKind::Union {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  let unionKind = &decl->kind as DeclKind::Union*;
  let declListPtr = &unionKind->subTypes;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    let tag = newLocDecl(state, DeclKind::Struct {});
    parseSubStruct(state, tag);

    // Use 'arg' of the struct type to point to the parent type.
    let structType = &tag->type->kind as TypeKind::Struct*;
    structType->parent = decl->type;

    // TODO: trailing comments?
    let newList = newDeclList(tag);
    *declListPtr = newList;
    declListPtr = &newList->next;
  }

  decl->endLocation = getNextToken(state).location;  // eat }

  return decl;
}


// func_decl :=
//  'func' identifier [ '->' type ] '(' [decl (',' decl)*] ')' compound_stmt?
func parseFuncDecl(state: ParseState*, isExtern: bool) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::Func {});
  (&decl->kind as DeclKind::Func*)->isExtern = isExtern;
  getNextToken(state);  // eat func

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  decl->type = newType(TypeKind::Func {});
  let funcType = decl->type->kind as TypeKind::Func*;

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);  // eat (

  let curType = decl->type;
  let firstParam: DeclAST* = null;
  let curParam: DeclAST* = null;
  while (!match(state, TokenKind::CLOSE_PAREN)) {
    if (match(state, TokenKind::ELLIPSIS)) {
      getNextToken(state);
      funcType->isVarargs = true;

      expect(state, TokenKind::CLOSE_PAREN);
      break;
    }

    let param = parseNameTypePair(state);
    if (firstParam == null) {
      firstParam = param;
      curParam = param;
    } else {
      curParam->next = param;
      curParam = param;
    }
    curType->next = param->type;
    curType = param->type;

    if (match(state, TokenKind::CLOSE_PAREN)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);    // eat ,
  }
  getNextToken(state);  // eat )

  (&decl->kind as DeclKind::Func*)->args = firstParam;

  funcType->args = decl->type->next;
  decl->type->next = null;

  if (match(state, TokenKind::PTR_OP)) {
    getNextToken(state);    // eat ->
    funcType->result = parseType(state);
  } else {
    funcType->result = newType(TypeKind::Void {});
  }

  let funcKind = &decl->kind as DeclKind::Func*;
  if (!funcKind->isExtern) {
    expect(state, TokenKind::OPEN_BRACE);
    funcKind->body = parseCompoundStmt(state);
    decl->endLocation = funcKind->body->endLocation;
  } else {
    expect(state, TokenKind::SEMICOLON);
    decl->endLocation = getNextToken(state).location;    // eat ;
  }

  addTrailingCommentsDecl(state, decl);

  return decl;
}

func parseImportDecl(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::Import {});
  getNextToken(state);  // eat import

  expect(state, TokenKind::IDENTIFIER);

  let ident = getNextToken(state);
  let expr = newExpr(ExprKind::Variable {
    identifier = ident,
  });

  while (match(state, TokenKind::DOT)) {
    let op = getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    let identifier = getNextToken(state);
    expr = newExpr(ExprKind::Member {
      object = expr,
      op = op,
      identifier = identifier,
      fieldIndex = -1,
    });
  }

  (&decl->kind as DeclKind::Import*)->path = expr;
  expect(state, TokenKind::SEMICOLON);
  decl->endLocation = getNextToken(state).location;

  addTrailingCommentsDecl(state, decl);

  return decl;
}

func parseLetDecl(state: ParseState*, isExtern: bool) -> DeclAST* {
  let decl = parseVarDecl(state);
  if (let varKind = &decl->kind as DeclKind::Var*) {
    varKind->isExtern = isExtern;
    if (isExtern && varKind->init != null) {
      failParse(state, "Extern let cannot have init");
    }
  } else if (isExtern) {
    failParse(state, "Cannot have extern const");
  }

  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  addTrailingCommentsDecl(state, decl);
  return decl;
}

func parseDecl(state: ParseState*) -> DeclAST* {
  if (match(state, TokenKind::EXTERN)) {
    getNextToken(state);

    if (match(state, TokenKind::FUNC)) {
      return parseFuncDecl(state, true);
    }

    if (match(state, TokenKind::LET)) {
      return parseLetDecl(state, true);
    }

    failParse(state, "Expected func or let");
  }

  if (match(state, TokenKind::FUNC)) {
    return parseFuncDecl(state, false);
  }

  if (match(state, TokenKind::LET) || match(state, TokenKind::CONST)) {
    return parseLetDecl(state, false);
  }

  if (match(state, TokenKind::STRUCT)) {
    let decl = parseStruct(state);

    optionalSemicolon(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::ENUM)) {
    let decl = parseEnum(state);

    optionalSemicolon(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::UNION)) {
    let decl = parseUnion(state);

    optionalSemicolon(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::IMPORT)) {
    return parseImportDecl(state);
  }

  failParse(state, "Unknown declaration");
  return null;
}
