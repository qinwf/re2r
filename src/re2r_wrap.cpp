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
#include "../inst/include/re2/regexp.h"
#include <tuple>
#include <memory>

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

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

XPtr<RE2Obj> cpp_re2_compile_one(string pattern,
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

    auto options = unique_ptr<RE2::Options>(new RE2::Options());

    RE2::Options::Encoding enc_value;
    enc_value = (utf_8_value = true) ? RE2::Options::EncodingUTF8 : RE2::Options::EncodingLatin1;
    options->set_encoding(enc_value);

    options->set_log_errors(log_errors_value);
    options->set_posix_syntax(posix_syntax_value);
    options->set_case_sensitive(case_sensitive_value);
    options->set_dot_nl(dot_nl_value);
    options->set_literal(literal_value);
    options->set_longest_match(longest_match_value);
    options->set_max_mem(max_mem_value);
    options->set_never_nl(never_nl_value);
    options->set_never_capture(never_capture_value);

    if(options->posix_syntax() == true){
        options->set_one_line(one_line_value);
        options->set_perl_classes(perl_classes_value);
        options->set_word_boundary(word_boundary_value);
    }

    auto regobj =
        XPtr<RE2Obj>(
            new RE2Obj(
                    move(options),
                    pattern
            )
        );
    RE2* ptr = &regobj->regexp;
    if (!ptr->ok()) {
        // long code = (long) regexp->error_code();
        // const std::string &msg = regexp->error();
        check_compile_error(ptr->error_code(), ptr->error_arg());
    }

    return regobj;
}

inline LogicalVector btd(bool input){
    return wrap(input);
}

// [[Rcpp::export]]
SEXP cpp_get_options(XPtr<RE2Obj>& ptr){
    List rptr(13);
    auto re2ptr = ptr.get();
    unique_ptr<RE2::Options>& opt = re2ptr->options;
    rptr[0] = btd(opt->utf8());
    rptr[1] = btd(opt->posix_syntax());
    rptr[2] = btd(opt->case_sensitive());
    rptr[3] = btd(opt->dot_nl());
    rptr[4] = btd(opt->literal());
    rptr[5] = btd(opt->longest_match());
    rptr[6] = btd(opt->never_nl());
    rptr[7] = btd(opt->never_capture());
    rptr[8] = btd(opt->one_line());
    rptr[9] = btd(opt->perl_classes());
    rptr[10] = btd(opt->word_boundary());
    rptr[11] = btd(opt->log_errors());
    rptr[12] = (double) opt->max_mem();
    return rptr;
}

// [[Rcpp::export]]
SEXP cpp_re2_compile(CharacterVector input,
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
    if (input.size() == 0){
        return R_NilValue;
    }
    if (input.size() == 1){
        if (*input.begin() == NA_STRING){
            return R_NilValue;
        }else {

            Shield<SEXP> res(cpp_re2_compile_one(R_CHAR(STRING_ELT(input, 0)),log_errors_value,utf_8_value,
                                                 posix_syntax_value,case_sensitive_value,
                                                 dot_nl_value,literal_value,
                                                 longest_match_value,never_nl_value,
                                                 never_capture_value,one_line_value,
                                                 perl_classes_value,word_boundary_value,
                                                 max_mem_value));
            Rf_setAttrib( res, R_ClassSymbol, Rf_mkString("re2c") );
            return res;
        }
    }else{
        Shield<SEXP> resx(Rf_allocVector(VECSXP, input.size()));
        SEXP res = resx;
        SEXP inputx = input;
        auto re2c = Rf_mkString("re2c");
        for (auto it = 0 ; it != input.size(); it++){
            auto rstr = STRING_ELT(inputx, it);

            if (rstr  == NA_STRING){
                SET_VECTOR_ELT(res,it, R_NilValue);
                continue;
            }
            auto r_char = R_CHAR(rstr);
            Shield<SEXP> resi(cpp_re2_compile_one(r_char,log_errors_value,utf_8_value,
                                        posix_syntax_value,case_sensitive_value,
                                        dot_nl_value,literal_value,
                                        longest_match_value,never_nl_value,
                                        never_capture_value,one_line_value,
                                        perl_classes_value,word_boundary_value,
                                        max_mem_value));
            Rf_setAttrib( resi, R_ClassSymbol,  re2c);
            SET_VECTOR_ELT(res, it, resi);
        }
        return res;
    }

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
int get_expression_size(XPtr<RE2Obj>& regexp){
    return regexp->regexp.ProgramSize();
}

// [[Rcpp::export]]
SEXP cpp_get_pattern(XPtr<RE2Obj>& ptr){
    SEXP res = PROTECT( Rf_allocVector(STRSXP,1));
    string ress =ptr->regexp.pattern();
    SET_STRING_ELT(res, 0, Rf_mkCharLenCE( ress.c_str(),  strlen(ress.c_str()), CE_UTF8));
    UNPROTECT(1);
    return res;
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
int get_number_of_groups(XPtr<RE2Obj>& regexp){
    return regexp->regexp.NumberOfCapturingGroups();
}

// [[Rcpp::export]]
SEXP cpp_get_named_groups(XPtr<RE2Obj>& ptr){
    return wrap(get_groups_name(&ptr->regexp,ptr->regexp.NumberOfCapturingGroups()));
}

struct QuoteMetaP : public Worker
{
    // source
    optstring& input;
    // destination
    optstring& output;

    // initialize with source and destination
    QuoteMetaP(optstring&  input_, optstring& output_)
        : input(input_),output(output_) {}

    // the range of elements requested
    void operator()(std::size_t begin, std::size_t end) {
        RE2 tt("");
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       output.begin() + begin,
                       [this,&tt](tr2::optional<string>& x)->tr2::optional<string>{
                           if (!bool(x)){
                               return tr2::nullopt;
                           }
                           return tr2::make_optional( tt.QuoteMeta(x.value()));
                       });
    }
};

// [[Rcpp::export]]
SEXP cpp_quote_meta(CharacterVector input, bool parallel, size_t grain_size){

    if (!parallel || input.size() < grain_size){
        SEXP inputx = input;
        Shield<SEXP> ress(Rf_allocVector(STRSXP,input.size()));
        SEXP res = ress;
        RE2 tt(""); // break on windows without tt
        for(auto it = 0; it!= input.size(); it++){
            auto rstr = STRING_ELT(inputx, it);
            if (rstr == NA_STRING){
                SET_STRING_ELT(res, it, NA_STRING);
                continue;
            }

            auto resi = tt.QuoteMeta(R_CHAR(rstr));
            SET_STRING_ELT(res, it, Rf_mkCharLenCE(resi.c_str(),  strlen(resi.c_str()) , CE_UTF8));
        }

        return res;
    }
    else{
        optstring res(input.size());
        auto inputv = as_vec_opt_string(input);
        QuoteMetaP pobj(inputv, res);
        parallelFor(0, input.size(), pobj, grain_size);
        return toprotect_optstring_sexp(res);
    }
}

struct ReplaceP : public Worker
{
    optstring& input;
    RE2& tt;
    RE2::Options& opt;
    string& rewrite;

    ReplaceP(optstring&  input_, RE2& tt_, RE2::Options& opt_, string& rewrite_)
        : input(input_), tt(tt_), opt(opt_), rewrite(rewrite_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::for_each(input.begin() + begin,
                       input.begin() + end,
                       [this,&pattern](tr2::optional<string>& x){
                           if (!bool(x)){
                               return;
                           }
                           pattern.Replace(& x.value(), pattern, rewrite);

                           });
    }
};

struct ReplaceGlobalP : public Worker
{
    optstring& input;
    vector<size_t>& count;
    RE2& tt;
    RE2::Options& opt;
    string& rewrite;

    ReplaceGlobalP(optstring&  input_,vector<size_t>& count_, RE2& tt_, RE2::Options& opt_,string& rewrite_)
        : input(input_), count(count_), tt(tt_), opt(opt_),rewrite(rewrite_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       count.begin() + begin,
                      [this,&pattern](tr2::optional<string>& x){
                          if (!bool(x)){
                              return 0;
                          }
                          return pattern.GlobalReplace(&x.value(), pattern, rewrite);
                          });
    }
};



// [[Rcpp::export]]
SEXP cpp_replace(CharacterVector input, XPtr<RE2Obj>& regexp, string& rewrite, bool global_, bool parallel, size_t grain_size){
    string errmsg;
    auto inputv = as_vec_opt_string(input);
    RE2* ptr = &(regexp->regexp);

    if(! ptr->CheckRewriteString(rewrite, &errmsg)){
        throw ErrorRewriteString(errmsg);
    }

    if(!global_) {
        if (!parallel || input.size() < grain_size){
            for(tr2::optional<string>& ind : inputv) {
                if(!bool(ind)){
                    continue;
                }
                ptr->Replace(&ind.value(),*ptr,rewrite);
            }
            return toprotect_optstring_sexp(inputv);

        } else{

            optstring res(input.size());
            ReplaceP pobj(inputv, *ptr, *(regexp->options), rewrite);
            parallelFor(0, input.size(), pobj, grain_size);
            return   toprotect_optstring_sexp(inputv);
        }
    }
    else {
        vector<size_t> count;
        if (!parallel || input.size() < grain_size){
            count.reserve(input.size());
            for(tr2::optional<string>& ind : inputv) {
                if(!bool(ind)){
                    count.push_back(0);
                }
                count.push_back(ptr->GlobalReplace(&ind.value(),*ptr,rewrite));
            }
        } else {
            count.resize(input.size());
            ReplaceGlobalP pobj(inputv, count, *ptr, *(regexp->options), rewrite);
            parallelFor(0, input.size(), pobj, grain_size);
        }

        CharacterVector res( toprotect_optstring_sexp(inputv));
        res.attr("count") = count;
        return res;
    }
}

struct ExtractP : public Worker
{
    optstring& input;
    optstring& output;
    RE2& tt;
    RE2::Options& opt;

    ExtractP(optstring&  input_, optstring& output_, RE2& tt_, RE2::Options& opt_)
        : input(input_), output(output_), tt(tt_), opt(opt_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       output.begin() + begin,
                       [this, &pattern](tr2::optional<string>& x) -> tr2::optional<string>{
                           if(!bool(x)){
                               return tr2::nullopt;
                           }
                           StringPiece match;
                           if (! pattern.Match( x.value(), 0 , x.value().length(), RE2::UNANCHORED, &match, 1)) {
                               return tr2::nullopt;
                           } else {
                               return tr2::make_optional(match.as_string());
                           }

                           });
    }
};


struct ExtractAllP : public Worker
{
    optstring& input;
    vector<tr2::optional<vector<string>>>& output;
    RE2& tt;
    RE2::Options& opt;

    ExtractAllP(optstring&  input_, vector<tr2::optional<vector<string>>>& output_,
                RE2& tt_, RE2::Options& opt_)
        : input(input_), output(output_), tt(tt_), opt(opt_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       output.begin() + begin,
                       [this, &pattern](tr2::optional<string>& x) -> tr2::optional<vector<string>>{
                           if(!bool(x)){
                               return tr2::nullopt;
                           }
                           StringPiece match;
                           vector<string> res;

                           StringPiece str(x.value());
                           size_t lastIndex = 0;

                           while (pattern.Match(str, lastIndex , x.value().length(), RE2::UNANCHORED, &match, 1)){
                               lastIndex = match.data() - str.data() + match.size();
                               res.push_back(match.as_string());
                           }

                            return tr2::make_optional(res);
                       });
    }
};

// [[Rcpp::export]]
SEXP cpp_extract(CharacterVector input, XPtr<RE2Obj>& regexp, bool all, bool parallel, size_t grain_size){
    string errmsg;

    RE2* ptr = &(regexp->regexp);
    SEXP inputx = input;

    if (! parallel || input.size() < grain_size){

        R_xlen_t index = 0;

        StringPiece match;

        if (!all){

            Shield<SEXP>  xs(Rf_allocVector(STRSXP, input.size()));
            SEXP x = xs;

            for(auto it = 0; it!= input.size(); it++){

                auto rstr = STRING_ELT(inputx, it);
                if (rstr == NA_STRING){
                    SET_STRING_ELT(x, it, NA_STRING);
                    continue;
                }

                StringPiece str(R_CHAR(STRING_ELT(inputx, it)));
                auto str_size = strlen(R_CHAR(STRING_ELT(inputx, it)));
                size_t lastIndex = 0;
                if (! ptr->Match(str, lastIndex , str_size, RE2::UNANCHORED, &match, 1)) {
                    SET_STRING_ELT(x, index, NA_STRING);
                } else {
                    string mstring = match.as_string();
                    SET_STRING_ELT(x, index, Rf_mkCharLenCE(mstring.c_str(),  strlen(mstring.c_str()) , CE_UTF8));
                }
                index++;
            }
            return x;
        } else {
            Shield<SEXP>  xs(Rf_allocVector(VECSXP, input.size()));
            SEXP x = xs;

            for(auto it = 0; it!= input.size(); it++){

                auto rstr = STRING_ELT(inputx, it);
                if (rstr == NA_STRING){
                    SET_VECTOR_ELT(x, it, R_NilValue);
                    continue;
                }

                StringPiece str(R_CHAR(rstr));
                auto str_size = strlen(R_CHAR(rstr));
                size_t lastIndex = 0;
                vector<string> res;

                while (ptr->Match(str, lastIndex , str_size, RE2::UNANCHORED, &match, 1)){
                    lastIndex = match.data() - str.data() + match.size();
                    res.push_back(match.as_string());
                }

                if (res.empty()) {
                    SET_VECTOR_ELT(x, index, R_NilValue);
                }else{
                    SET_VECTOR_ELT(x, index, Shield<SEXP>(toprotect_vec_string_sexp(res)));
                }

                index++;
            }
            return x;
        }
    } else{ // parallel
        auto inputv = as_vec_opt_string(input);
        if (!all){
            optstring res(input.size());

            ExtractP pobj(inputv, res, *ptr, *(regexp->options));
            parallelFor(0, input.size(), pobj, grain_size);
            return toprotect_optstring_sexp(res);
        } else {
            vector<tr2::optional<vector<string>>> res(input.size());

            ExtractAllP pobj(inputv, res, *ptr, *(regexp->options));
            parallelFor(0, input.size(), pobj, grain_size);

            Shield<SEXP>  xs(Rf_allocVector(VECSXP, input.size()));
            SEXP x = xs;

            R_xlen_t index = 0;

            for (tr2::optional<vector<string>>& resi : res){
                if (! bool(resi) || resi.value().empty()) {
                    SET_VECTOR_ELT(x, index, R_NilValue);
                }
                else{
                    SET_VECTOR_ELT(x, index, Shield<SEXP>(toprotect_vec_string_sexp(resi.value())));
                }
                index ++;
            }
            return x;
        }

    }
}

template <typename T>
inline string numbertostring ( T Number )
{
    ostringstream ss;
    ss << Number;
    return ss.str();
}

// [[Rcpp::export]]
SEXP cpp_get_program_fanout(XPtr<RE2Obj>& regexp){
    map<int,int> res;
    regexp->regexp.ProgramFanout(&res);
    return(wrap(res));
}

// [[Rcpp::export]]
SEXP cpp_regex_to_string(XPtr<RE2Obj>& regexp){
    auto ptr = regexp->regexp.Regexp();
    string mstring = ptr->ToString();
    SEXP res = PROTECT( Rf_allocVector(STRSXP,1));
    SET_STRING_ELT(res, 0, Rf_mkCharLenCE(mstring.c_str(),  strlen(mstring.c_str()) , CE_UTF8));
    UNPROTECT(1);
    return(res);
}

// [[Rcpp::export]]
LogicalVector cpp_regex_mimicsPCRE(XPtr<RE2Obj>& regexp){
    auto ptr = regexp->regexp.Regexp();
    return wrap(ptr->MimicsPCRE());
}
