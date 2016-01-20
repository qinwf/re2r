#ifndef RE2R_RE2R_H
#define RE2R_RE2R_H

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include <re2/re2.h>
using re2::RE2;
using re2::StringPiece;

XPtr<RE2> re2_cpp_compile(const char* pattern,
                          bool log_errors_value,
                          bool utf_8_value,
                          bool posix_syntax_value,
                          bool case_sensitive_value,
                          bool dot_nl_value,
                          bool literal_value,
                          bool longest_match_value,
                          bool never_nl_value,
                          bool never_capture_value,
                          bool one_line_value,
                          bool perl_classes_value,
                          bool word_boundary_value,
                          int64_t max_mem_value);

#endif
