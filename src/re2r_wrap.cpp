#include <re2r.h>

// [[Rcpp::export]]
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
                          int64_t max_mem_value){
    RE2::Options options;

    RE2::Options::Encoding enc_value;
    enc_value = (utf_8_value =true) ? RE2::Options::EncodingUTF8 : RE2::Options::EncodingLatin1;
    options.set_encoding(enc_value);

    options.set_log_errors(log_errors_value);
    options.set_posix_syntax(posix_syntax_value);
    options.set_case_sensitive(case_sensitive_value);
    options.set_dot_nl(dot_nl_value);
    options.set_literal(literal_value);
    options.set_longest_match(longest_match_value);
    options.set_max_mem(max_mem_value);
    options.set_never_nl(never_nl_value);
    options.set_never_capture(never_capture_value);
    options.set_one_line(one_line_value);
    options.set_perl_classes(perl_classes_value);
    options.set_word_boundary(word_boundary_value);

    return XPtr<RE2>(
        new RE2(
                StringPiece(pattern, (int) strlen(pattern)),
                options
            )
        );
}

