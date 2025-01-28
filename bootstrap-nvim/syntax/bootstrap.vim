" Vim syntax file
" Language: bootstrap
if exists("b:current_syntax")
    finish
endif

let s:keepcpo = &cpo
set cpo&vim

syn keyword bootstrapKeyword
      \ break
      \ continue
      \ default
      \ else
      \ for
      \ if
      \ return
      \ switch
      \ while

syn keyword bootstrapImport skipwhite skipempty nextgroup=bootstrapImportModule
      \ import

syn keyword bootstrapFuncDefinition skipwhite skipempty nextgroup=bootstrapTypeName
      \ func

syn keyword bootstrapTypeDefinition skipwhite skipempty nextgroup=bootstrapTypeName
      \ enum
      \ struct
      \ union

syn keyword bootstrapVarDefinition skipwhite skipempty nextgroup=bootstrapVarName
      \ let
      \ var

syn keyword bootstrapBoolean
      \ false
      \ true

syn keyword bootstrapNull
      \ null
      \ NULL

syn match bootstrapImportModule contained nextgroup=bootstrapImportComponent
      \ /\<[A-Za-z_][A-Za-z_0-9]*\>/
syn match bootstrapImportComponent contained nextgroup=bootstrapImportComponent
      \ /\.\<[A-Za-z_][A-Za-z_0-9]*\>/

syn match bootstrapTypeName contained skipwhite skipempty nextgroup=bootstrapTypeParameters
      \ /\<[A-Za-z_][A-Za-z_0-9\.]*\>/
syn match bootstrapVarName contained skipwhite skipempty nextgroup=bootstrapTypeDeclaration
      \ /\<[A-Za-z_][A-Za-z_0-9]*\>/

" TypeName[Optionality]?
syn match bootstrapType contained skipwhite skipempty nextgroup=bootstrapTypeParameters
      \ /\<[A-Za-z_][A-Za-z_0-9\.]*\>[!?\*]\?/
syn match bootstrapType contained skipwhite skipempty nextgroup=bootstrapType
      \ /\<const\>/
" [Type:Type] (dictionary) or [Type] (array)
syn region bootstrapType contained contains=bootstrapTypePair,bootstrapType
      \ matchgroup=Delimiter start=/\[/ end=/\]/
syn match bootstrapTypePair contained skipwhite skipempty nextgroup=bootstrapTypeParameters,bootstrapTypeDeclaration
      \ /\<[A-Za-z_][A-Za-z_0-9\.]*\>[!?]\?/
" (Type[, Type]) (tuple)
" FIXME: we should be able to use skip="," and drop bootstrapParamDelim
syn region bootstrapType contained contains=bootstrapType,bootstrapParamDelim
      \ matchgroup=Delimiter start="[^@]\?(" end=")" matchgroup=NONE skip=","
syn match bootstrapParamDelim contained
      \ /,/
" <Generic Clause> (generics)
syn region bootstrapTypeParameters contained contains=bootstrapVarName
      \ matchgroup=Delimiter start="<" end=">" matchgroup=NONE skip=","

syn match bootstrapTypeAliasValue skipwhite skipempty nextgroup=bootstrapType
      \ /=/
syn match bootstrapTypeDeclaration skipwhite skipempty nextgroup=bootstrapType
      \ /:/
syn match bootstrapTypeDeclaration skipwhite skipempty nextgroup=bootstrapType
      \ /->/

syn match bootstrapKeyword
      \ /\<case\>/
syn region bootstrapCaseLabelRegion
      \ matchgroup=bootstrapKeyword start=/\<case\>/ matchgroup=Delimiter end=/:/ oneline contains=TOP
syn region bootstrapDefaultLabelRegion
      \ matchgroup=bootstrapKeyword start=/\<default\>/ matchgroup=Delimiter end=/:/ oneline

syn region bootstrapParenthesisRegion contains=TOP
      \ matchgroup=NONE start=/(/ end=/)/

syn region bootstrapString contains=bootstrapInterpolationRegion
      \ start=/"/ skip=/\\\\\|\\"/ end=/"/
syn region bootstrapInterpolationRegion contained contains=TOP
      \ matchgroup=bootstrapInterpolation start=/\\(/ end=/)/
syn region bootstrapLineComment contains=bootstrapTodo
      \ start="//" end="$"

syn match bootstrapDecimal
      \ /[+\-]\?\<\([0-9][0-9_]*\)\([.][0-9_]*\)\?\([eE][+\-]\?[0-9][0-9_]*\)\?\>/
syn match bootstrapHex
      \ /[+\-]\?\<0x[0-9A-Fa-f][0-9A-Fa-f_]*\(\([.][0-9A-Fa-f_]*\)\?[pP][+\-]\?[0-9][0-9_]*\)\?\>/
syn match bootstrapOct
      \ /[+\-]\?\<0o[0-7][0-7_]*\>/
syn match bootstrapBin
      \ /[+\-]\?\<0b[01][01_]*\>/

syn match bootstrapOperator skipwhite skipempty nextgroup=bootstrapTypeParameters
      \ "\.\@<!\.\.\.\@!\|[/=\-+*%<>!&|^~]\@<!\(/[/*]\@![/=\-+*%<>!&|^~]*\|*/\@![/=\-+*%<>!&|^~]*\|->\@![/=\-+*%<>!&|^~]*\|[=+%<>!&|^~][/=\-+*%<>!&|^~]*\)"
syn match bootstrapOperator skipwhite skipempty nextgroup=bootstrapTypeParameters
      \ "\.\.[<.]"

syn match bootstrapChar
      \ /'\([^'\\]\|\\\(["'tnr0\\]\|x[0-9a-fA-F]\{2}\|u[0-9a-fA-F]\{4}\|U[0-9a-fA-F]\{8}\)\)'/

syn match bootstrapTupleIndexNumber contains=bootstrapDecimal
      \ /\.[0-9]\+/
syn match bootstrapDecimal contained
      \ /[0-9]\+/


syn keyword bootstrapTodo MARK TODO FIXME contained

syn match bootstrapCastOp skipwhite skipempty nextgroup=bootstrapType
      \ "\<as\>"



hi def link bootstrapImport Include
hi def link bootstrapImportModule Title
hi def link bootstrapImportComponent Identifier
hi def link bootstrapKeyword Statement
hi def link bootstrapTypeDefinition Define
hi def link bootstrapType Type
hi def link bootstrapTypePair Type
hi def link bootstrapTypeAliasName Identifier
hi def link bootstrapTypeName Function
hi def link bootstrapFuncDefinition Define
hi def link bootstrapVarDefinition Define
hi def link bootstrapVarName Identifier
hi def link bootstrapTypeAliasValue Delimiter
hi def link bootstrapTypeDeclaration Delimiter
hi def link bootstrapTypeParameters Delimiter
hi def link bootstrapBoolean Boolean
hi def link bootstrapString String
hi def link bootstrapInterpolation Special
hi def link bootstrapLineComment Comment
hi def link bootstrapDecimal Number
hi def link bootstrapHex Number
hi def link bootstrapOct Number
hi def link bootstrapBin Number
hi def link bootstrapOperator Function
hi def link bootstrapChar Character
hi def link bootstrapTodo Todo
hi def link bootstrapNull Constant
hi def link bootstrapCastOp Operator

let b:current_syntax = "bootstrap"

let &cpo = s:keepcpo
unlet s:keepcpo
