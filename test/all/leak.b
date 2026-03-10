// UNSUPPORTED: asan
//
// RUN: %if system-darwin %{ \
// RUN: timeout 5 /usr/bin/leaks --atExit --  %raw_bootstrap %s \
// RUN: %} %else %{ \
// RUN: timeout 5 valgrind --error-exitcode=1 --leak-check=full %raw_bootstrap %s \
// RUN: %}
func main() {

}
