// RUN: split-file %s %t
// RUN: printf 'import bar;\nfunc foo() {}\n' | %brio -format - | cmp %t/mixed_kinds.b -
// RUN: printf 'let x = 12;\nlet y = 13;\n' | %brio -format - | cmp %t/same_kinds.b -
// RUN: printf 'import a;\nimport b;\nimport c;\n\n\n\nimport d;\n\nimport e;' | %brio -format - | cmp %t/imports.b -
//
//--- mixed_kinds.b
import bar;

func foo() {

}
//--- same_kinds.b
let x = 12;

let y = 13;
//--- imports.b
import a;
import b;
import c;


import d;

import e;
