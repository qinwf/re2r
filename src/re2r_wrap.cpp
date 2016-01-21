#include <re2r.h>

#define thr(code) case RE2::ErrorCode::code: throw code(msg); break;

void check_compile_error(RE2::ErrorCode code_,const string& msg){
    switch(code_){
        case RE2::ErrorCode::NoError: return; break;
        thr(ErrorInternal)
        thr(ErrorBadEscape)
        thr(ErrorBadCharClass)
        thr(ErrorBadCharRange)
        thr(ErrorMissingBracket)
        thr(ErrorMissingParen)
        thr(ErrorTrailingBackslash)
        thr(ErrorRepeatArgument)
        thr(ErrorRepeatSize)
        thr(ErrorRepeatOp)
        thr(ErrorBadPerlOp)
        thr(ErrorBadUTF8)
        thr(ErrorBadNamedCapture)
        thr(ErrorPatternTooLarge)
        default: return; break;
    }
}

// [[Rcpp::export]]
XPtr<RE2> cpp_re2_compile(const char * pattern,
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
                          int64_t max_mem_value)
{
    RE2::Options options;

    RE2::Options::Encoding enc_value;
    enc_value = (utf_8_value = true) ? RE2::Options::EncodingUTF8 : RE2::Options::EncodingLatin1;
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

    if(options.posix_syntax() == true){
        options.set_one_line(one_line_value);
        options.set_perl_classes(perl_classes_value);
        options.set_word_boundary(word_boundary_value);
    }

    XPtr<RE2> regexp =
        XPtr<RE2>(
            new RE2(StringPiece(pattern,
                                (int) strlen(pattern)),
                    options
            )
        );

    if (!regexp->ok()) {
        // long code = (long) regexp->error_code();
        // const std::string &msg = regexp->error();
        check_compile_error(regexp->error_code(), regexp->error_arg());
    }

    return regexp;
}


//' Get pre-compiled regular expression program size
//'
//' Returns the program size, a very approximate measure of a regexp's "cost".
//' Larger numbers are more expensive than smaller numbers.
//'
//' @param regexp a pre-compiled regular expression
//' @return a integer
//' @examples
//' regexp = re2_compile("1")
//' get_programsize(regexp)
//' @export
// [[Rcpp::export]]
int get_programsize(XPtr<RE2> regexp){
    return regexp->ProgramSize();
}


//' @rdname get_pattern
//' @export
// [[Rcpp::export]]
string cpp_get_pattern(XPtr<RE2> regexp){
    return regexp->pattern();
}

//' Return the number of capturing subpatterns
//'
//' Return the number of capturing subpatterns, or -1 if the
//' regexp wasn't valid on construction.  The overall match ($0)
//' does not count: if the regexp is "(a)(b)", returns 2.
//'
//' @param regexp a pre-compiled regular expression
//' @return a integer
//' @examples
//' regexp = re2_compile("1")
//' get_numberofcapturinggroups(regexp)
//' @export
// [[Rcpp::export]]
int get_numberofcapturinggroups(XPtr<RE2> regexp){
    return regexp->NumberOfCapturingGroups();
}