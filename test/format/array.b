// RUN: echo 'let x = {1, 2,};' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | diff %t -
// RUN: echo 'let x = {1, 2};' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | diff %t -
// RUN: echo 'let x = [1, 2,];' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | diff %t -
// RUN: echo 'let x = [1, 2,];' | %bootstrap -format - -o %t
// RUN: echo 'let x = [ 1, 2 ];'  | diff %t -

