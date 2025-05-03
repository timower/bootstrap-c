import ast;

import state;
import token;
import type;

import expr;
import stmt;


// Add any comments in state that are on the same line as decl to decl.
func addTrailingCommentsDecl(state: ParseState*, decl: DeclAST*) {
  let comments = getLineComments(state, decl->endLocation.line);
  if (comments == null) {
    return;
  }

  decl->comments = appendComments(decl->comments, comments);
}


// type_name_pair := identifier ':' type
func parseNameTypePair(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::VAR);

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  expect(state, TokenKind::COLON);
  getNextToken(state);

  decl->type = parseType(state);
  decl->endLocation = getLocation(state);

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

  decl->kind = DeclKind::STRUCT;

  // parse the fields
  let fields = decl;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    fields->next = parseNameTypePair(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);    // eat ;

    addTrailingCommentsDecl(state, fields->next);

    fields = fields->next;
  }
  decl->endLocation = getLocation(state);
  getNextToken(state);  // eat }

  fields->next = null;
  decl->fields = decl->next;
  decl->next = null;
}


// struct := 'struct' identifier '{' decl* '}'
func parseStruct(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::STRUCT);
  getNextToken(state);  // eat struct
  parseSubStruct(state, decl);
  return decl;
}


// enum := 'enum' identifier '{' identifier  (',' identifier )* ','? '}'
func parseEnum(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::ENUM);
  getNextToken(state);

  expect(state, TokenKind::IDENTIFIER);

  decl->type = newType(TypeKind::Enum {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  // parse constants
  let fields = decl;
  let idx = 0;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    expect(state, TokenKind::IDENTIFIER);

    let field = newLocDecl(state, DeclKind::ENUM_FIELD);
    field->type = getInt32();

    field->name = getNextToken(state);
    field->enumValue = idx++;

    fields->next = field;
    fields = field;

    field->endLocation = getLocation(state);

    if (match(state, TokenKind::CLOSE_BRACE)) {
      addTrailingCommentsDecl(state, field);
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);

    addTrailingCommentsDecl(state, field);
  }
  decl->endLocation = getLocation(state);
  getNextToken(state);  // eat }

  fields->next = null;
  decl->fields = decl->next;
  decl->next = null;
  return decl;
}

func parseUnion(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::UNION);
  getNextToken(state);  // eat 'union'

  expect(state, TokenKind::IDENTIFIER);
  decl->type = newType(TypeKind::Union {
    tag = getNextToken(state),
  });

  expect(state, TokenKind::OPEN_BRACE);
  getNextToken(state);

  let declListPtr = &decl->subTypes;
  while (!match(state, TokenKind::CLOSE_BRACE)) {
    let tag = newLocDecl(state, DeclKind::STRUCT);
    parseSubStruct(state, tag);

    // Use 'arg' of the struct type to point to the parent type.
    let structType = tag->type->kind as TypeKind::Struct*;
    structType->parent = decl->type;

    // TODO: trailing comments?
    let newList = newDeclList(tag);
    *declListPtr = newList;
    declListPtr = &newList->next;
  }

  decl->endLocation = getLocation(state);
  getNextToken(state);  // eat }

  return decl;
}


// func_decl :=
//  'func' identifier [ '->' type ] '(' [decl (',' decl)*] ')' compound_stmt?
func parseFuncDecl(state: ParseState*, isExtern: bool) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::FUNC);
  decl->isExtern = isExtern;
  getNextToken(state);  // eat func

  expect(state, TokenKind::IDENTIFIER);
  decl->name = getNextToken(state);

  decl->type = newType(TypeKind::Func {});
  let funcType = decl->type->kind as TypeKind::Func*;

  expect(state, TokenKind::OPEN_PAREN);
  getNextToken(state);  // eat (

  let curType = decl->type;
  let curDecl = decl;
  while (!match(state, TokenKind::CLOSE_PAREN)) {
    if (match(state, TokenKind::ELLIPSIS)) {
      getNextToken(state);
      funcType->isVarargs = true;

      expect(state, TokenKind::CLOSE_PAREN);
      break;
    }

    let param = parseNameTypePair(state);
    curDecl->next = param;
    curDecl = param;
    curType->next = param->type;
    curType = param->type;

    if (match(state, TokenKind::CLOSE_PAREN)) {
      break;
    }

    expect(state, TokenKind::COMMA);
    getNextToken(state);    // eat ,
  }
  getNextToken(state);  // eat )

  decl->fields = decl->next;
  decl->next = null;

  funcType->args = decl->type->next;
  decl->type->next = null;

  if (match(state, TokenKind::PTR_OP)) {
    getNextToken(state);    // eat ->
    funcType->result = parseType(state);
  } else {
    funcType->result = newType(TypeKind::Void {});
  }

  if (!decl->isExtern) {
    expect(state, TokenKind::OPEN_BRACE);
    decl->body = parseCompoundStmt(state);
    decl->endLocation = decl->body->endLocation;
  } else {
    expect(state, TokenKind::SEMICOLON);
    decl->endLocation = getLocation(state);
    getNextToken(state);    // eat ;
  }

  addTrailingCommentsDecl(state, decl);

  return decl;
}

func parseImportDecl(state: ParseState*) -> DeclAST* {
  let decl = newLocDecl(state, DeclKind::IMPORT);
  getNextToken(state);  // eat import

  expect(state, TokenKind::IDENTIFIER);

  let ident = getNextToken(state);
  let expr = newExpr(ExprKind::VARIABLE);
  expr->identifier = ident;

  while (match(state, TokenKind::DOT)) {
    let member = newExpr(ExprKind::MEMBER);
    member->lhs = expr;
    member->op = getNextToken(state);
    expect(state, TokenKind::IDENTIFIER);
    member->identifier = getNextToken(state);
    expr = member;
  }

  decl->init = expr;
  decl->endLocation = getLocation(state);
  expect(state, TokenKind::SEMICOLON);
  getNextToken(state);

  addTrailingCommentsDecl(state, decl);

  return decl;
}

func parseLetDecl(state: ParseState*, isExtern: bool) -> DeclAST* {
  let decl = parseVarDecl(state);
  decl->isExtern = isExtern;
  if (isExtern && decl->init != null) {
    failParse(state, "Extern let cannot have init");
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

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::ENUM)) {
    let decl = parseEnum(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::UNION)) {
    let decl = parseUnion(state);

    expect(state, TokenKind::SEMICOLON);
    getNextToken(state);

    addTrailingCommentsDecl(state, decl);

    return decl;
  }

  if (match(state, TokenKind::IMPORT)) {
    return parseImportDecl(state);
  }

  failParse(state, "Unknown declaration");
  return null;
}
