// RUN: %bootstrap %s | lli
// Test bool type and logical operations

func main() -> i32 {
    let trueVal = true;
    let falseVal = false;
    
    if (trueVal && falseVal) {
        return 1;
    }
    
    if (trueVal || falseVal) {
        let a = 10;
        let b = 5;
        if (a > b && a >= b && a != b) {
            if (!(a < b) && !(a <= b) && !(a == b)) {
                return 0;
            }
        }
    }
    
    return 2;
}