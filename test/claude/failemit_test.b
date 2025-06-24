// Test to trigger failEmit() function
// This test attempts to trigger register allocation issues during emission
// RUN: not %bootstrap -emit-asm %s 2>&1 | FileCheck %s

func main() -> i32 {
    // This complex nested expression should stress the register allocator
    let a = 1;
    let b = 2;
    let c = 3;
    let d = 4;
    let e = 5;
    let f = 6;
    let g = 7;
    let h = 8;
    let i = 9;
    let j = 10;
    let k = 11;
    let l = 12;
    let m = 13;
    let n = 14;
    let o = 15;
    let p = 16;
    let q = 17;
    let r = 18;
    let s = 19;
    let t = 20;
    let u = 21;
    let v = 22;
    let w = 23;
    let x = 24;
    let y = 25;
    let z = 26;
    
    // Complex nested arithmetic that might stress register allocation
    let result = ((((a + b) * (c + d)) + ((e + f) * (g + h))) + 
                  (((i + j) * (k + l)) + ((m + n) * (o + p)))) +
                 ((((q + r) * (s + t)) + ((u + v) * (w + x))) +
                  (((y + z) * (a + b)) + ((c + d) * (e + f))));
    
    return result;
}

// CHECK: Emit error: