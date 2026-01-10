// RUN: split-file %s %t
// RUN: not %bootstrap %t/unknown.b 2>&1 | grep "Unknown primary expression"
// RUN: not %bootstrap %t/let1.b 2>&1 | grep "Extern let cannot have init"
//
// RUN: not %bootstrap %t/empty.b 2>&1 | grep "Failed to parse file"
// RUN: not %bootstrap %t/empty_comment.b 2>&1 | grep "Failed to parse file"
// RUN: not %bootstrap %t/empty_str.b 2>&1 | grep "Failed to parse file"
// RUN: not %bootstrap %t/empty_char.b 2>&1 | grep "Failed to parse file"
// RUN: not %bootstrap %t/empty_prepro.b 2>&1 | grep "Failed to parse file"
//
// RUN: not %bootstrap %t/unknown_tok.b 2>&1 | grep "Unknown token"
//
// RUN: not %bootstrap %t/invalid_hex1.b 2>&1 | grep "Expected: ;"
// RUN: not %bootstrap %t/invalid_hex2.b 2>&1 | grep "Expected: ;"
// RUN: not %bootstrap %t/invalid_int.b 2>&1 | grep "Invalid integer"
//
// RUN: not %bootstrap %t/unknown_decl.b 2>&1 | grep "Unknown declaration"
//
// RUN: echo -n '#foo' | not %bootstrap - 2>&1 | grep 'Failed to parse file'
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

//--- invalid_int.b
let x = 12b34;

//--- unknown_decl.b
foo;
