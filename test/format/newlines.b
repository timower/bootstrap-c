// RUN: split-file %s %t
// RUN: printf 'let x = 12;\nfunc foo() {}\n' | %bootstrap -format - | diff %t/mixed_kinds.b -
// RUN: printf 'let x = 12;\nlet y = 13;\n' | %bootstrap -format - | diff %t/same_kinds.b -
// RUN: printf 'import a;\nimport b;\nimport c;\n' | %bootstrap -format - | diff %t/imports.b -
//
//--- mixed_kinds.b
let x = 12;

func foo() {

}
//--- same_kinds.b
let x = 12;

let y = 13;
//--- imports.b
import a;
import b;
import c;
