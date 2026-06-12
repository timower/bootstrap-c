// RUN: split-file %s %t
// RUN: not %brio %t/unknown.b 2>&1 | grep "Unknown primary expression"
// RUN: not %brio %t/let1.b 2>&1 | grep "Extern let cannot have init"
//
// RUN: not %brio %t/empty.b 2>&1 | grep "Failed to parse file"
// RUN: not %brio %t/empty_comment.b 2>&1 | grep "Failed to parse file"
// RUN: not %brio %t/empty_str.b 2>&1 | grep "Failed to parse file"
// RUN: not %brio %t/empty_char.b 2>&1 | grep "Failed to parse file"
// RUN: not %brio %t/empty_prepro.b 2>&1 | grep "Failed to parse file"
//
// RUN: not %brio %t/unknown_tok.b 2>&1 | grep "Unknown token"
//
// RUN: not %brio %t/invalid_hex1.b 2>&1 | grep "Expected: ;"
// RUN: not %brio %t/invalid_hex2.b 2>&1 | grep "Expected: ;"
// RUN: not %brio %t/invalid_hex3.b 2>&1 | grep "Expected: ;"
// RUN: not %brio %t/invalid_int.b 2>&1 | grep "Invalid integer"
//
// RUN: not %brio %t/unknown_decl.b 2>&1 | grep "Unknown declaration"
//
// RUN: not %brio %t/negative_size.b 2>&1 | grep "Expected positive size"
// RUN: not %brio %t/no_size.b 2>&1 | grep "Expected"
//
// RUN: echo -n '#foo' | not %brio - 2>&1 | grep 'Failed to parse file'
// RUN: echo 'const x = 2#/1;' | not %brio - 2>&1 | grep 'Unknown token'

//
//--- unknown.b
func foo() {
  import;
}

//--- empty.b

//--- empty_comment.b

// test

//--- empty_str.b

"test

//--- empty_char.b

't


//--- empty_prepro.b

#test

//--- let1.b
extern let x = 1;

//--- unknown_tok.b
@test = 12;

//--- invalid_hex1.b
let x = 0xABCDEFG;

//--- invalid_hex2.b
let x = 0xabcdefg;

//--- invalid_hex3.b
let x = 1xabcdefg;

//--- invalid_int.b
let x = 12b34;

//--- unknown_decl.b
foo;

//--- negative_size.b
let x: i32[-1] = [];

//--- no_size.b
extern let x: i8[];
