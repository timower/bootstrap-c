// RUN: echo 'let x = {1, 2,};' | %brio -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -
// RUN: echo 'let x = {1, 2};' | %brio -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -
// RUN: echo 'let x = [1, 2,];' | %brio -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -
// RUN: echo 'let x = [1, 2,];' | %brio -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -

