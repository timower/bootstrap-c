// RUN: echo 'let x = {1, 2,};' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -
// RUN: echo 'let x = {1, 2};' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -
// RUN: echo 'let x = [1, 2,];' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -
// RUN: echo 'let x = [1, 2,];' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | cmp %t -

