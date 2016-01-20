#include <re2r.h>

// [[Rcpp::export]]
XPtr<RE2> re2_cpp_compile(const char* pattern,bool log_errors_value){
    RE2::Options options;
    options.set_log_errors(log_errors_value);
    return XPtr<RE2>(
        new RE2(
                StringPiece(pattern, (int) strlen(pattern)),
                options
            )
        );
}

