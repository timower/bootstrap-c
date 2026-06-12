// UNSUPPORTED: asan
//
// RUN: %if system-darwin %{ \
// RUN: timeout 5 /usr/bin/leaks --atExit --  %raw_brio %s \
// RUN: %} %else %{ \
// RUN: timeout 5 valgrind --error-exitcode=1 --leak-check=full %raw_brio -stdlib %root_dir %s \
// RUN: %}
func main() {

}
