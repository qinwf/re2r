// This file is part of the 're2r' package for R.
// Copyright (C) 2016, Qin Wenfeng
// All rights reserved.

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:

// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.

// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.

// 3. Neither the name of the copyright holder nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
// BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "../inst/include/re2r.h"

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
//' regexp = re2("1")
//' get_expression_size(regexp)
//' @export
// [[Rcpp::export]]
int get_expression_size(XPtr<RE2>& regexp){
    return regexp->ProgramSize();
}

// [[Rcpp::export]]
string cpp_get_pattern(XPtr<RE2>& regexp){
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
//' regexp = re2("1")
//' get_number_of_groups(regexp)
//' @export
// [[Rcpp::export]]
int get_number_of_groups(XPtr<RE2>& regexp){
    return regexp->NumberOfCapturingGroups();
}

// [[Rcpp::export]]
IntegerVector cpp_get_named_groups(XPtr<RE2>& regexp){
    return wrap(regexp->NamedCapturingGroups());
}

// [[Rcpp::export]]
CharacterVector cpp_quote_meta(vector<string>& input){
    RE2 tt(""); // break on windows without tt
    vector<string> res;
    res.reserve(input.size());
    for(auto ind : input) res.push_back(tt.QuoteMeta(ind));
    return wrap(res);
}

// [[Rcpp::export]]
CharacterVector cpp_replace(XPtr<RE2>& regexp, string& rewrite, vector<string>& input, bool global_){
    string errmsg;

    if(!regexp->CheckRewriteString(rewrite, &errmsg)){
        throw ErrorRewriteString(errmsg);
    }

    if(!global_) for(string& ind : input) regexp->Replace(&ind,*regexp,rewrite);
    else         for(string& ind : input) regexp->GlobalReplace(&ind,*regexp,rewrite);
    return wrap(input);
}

// [[Rcpp::export]]
CharacterVector cpp_extract(XPtr<RE2>& regexp, string& rewrite, vector<string>& input){
    string errmsg;

    if(!regexp->CheckRewriteString(rewrite, &errmsg)){
        throw ErrorRewriteString(errmsg);
    }
    vector<string> res(input.size());
    auto res_iter = res.begin();
    for(const string& ind : input) {
        if (! regexp->Extract(ind,*regexp,rewrite,&(*res_iter))) *res_iter="";
        res_iter+=1;
    }

    return wrap(res);
}
