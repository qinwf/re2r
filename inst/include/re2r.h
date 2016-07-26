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


#ifndef RE2R_RE2R_H
#define RE2R_RE2R_H

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;


#include <memory>
#include <tuple>

#include <re2/regexp.h>
#include <re2/re2.h>

using re2::RE2;
using re2::StringPiece;

#include "optional.hpp"
#include <memory>
namespace tr2 = std::experimental;

typedef vector<tr2::optional<string>> optstring;


#define INVALID_ERROR_STRING                                                   \
"Invalid pointer for RE2 object. Please create a new RE2 object when R is "    \
"restarted."

#define INIT_ptr \
if(R_ExternalPtrAddr(regexp) == nullptr){ \
    stop(INVALID_ERROR_STRING); \
} \
OptRE2 *ptr = as<XPtr<OptRE2>>(regexp).get();

// exception

#define RCPP_EXCEPTION_CLASS(__CLASS__,__WHAT__)                               \
    class __CLASS__ : public std::exception{                                       \
    public:                                                                        \
        __CLASS__( const std::string messages ) throw() : message( __WHAT__ ){} ;  \
        virtual ~__CLASS__() throw(){} ;                                           \
        virtual const char* what() const throw() { return message.c_str() ; }      \
    private:                                                                       \
        std::string message ;                                                      \
    } ;

#define RCPP_SIMPLE_EXCEPTION_CLASS(__CLASS__,__MESSAGE__)                     \
    class __CLASS__ : public std::exception{                                       \
    public:                                                                        \
        __CLASS__() throw() {} ;                                                   \
        virtual ~__CLASS__() throw(){} ;                                           \
        virtual const char* what() const throw() { return __MESSAGE__ ; }          \
    } ;

RCPP_EXCEPTION_CLASS(ErrorInternal, std::string("unexpected error:") + messages)

RCPP_EXCEPTION_CLASS(ErrorBadEscape, std::string("bad escape sequence: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorBadCharRange, std::string("bad character class range: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorBadCharClass, std::string("bad character class: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorMissingBracket, std::string("missing closing ]: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorMissingParen, std::string("missing closing ): ") + messages)

RCPP_EXCEPTION_CLASS(ErrorTrailingBackslash, std::string("trailing \\ at end of regexp: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorRepeatArgument, std::string("repeat argument: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorRepeatSize, std::string("bad repetition argument: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorRepeatOp, std::string("bad repetition operator: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorBadPerlOp, std::string("bad perl operator: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorBadUTF8, std::string("invalid UTF-8 in regexp: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorBadNamedCapture, std::string("bad named capture group: ") + messages)

RCPP_EXCEPTION_CLASS(ErrorPatternTooLarge, std::string("pattern too large (compile failed): ") + messages)

RCPP_EXCEPTION_CLASS(ErrorRewriteString, std::string("rewrite string error: ") + messages)

// RCPP_EXCEPTION_CLASS(ErrorAnchorType, std::string("anchor type error: ") + messages)

XPtr<RE2> cpp_re2_compile(const char* pattern,
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


SEXP toprotect_optstring_sexp(const optstring& input);
SEXP toprotect_vec_string_sexp(const vector<string>& input);

SEXP toprotect_optstring_to_list_charmat(const optstring &optinner, size_t cols,
                                         SEXP groups_name);

SEXP toprotect_vec_optstring_to_charmat(const vector<optstring> &res,
                                        int cap_nums);
SEXP toprotect_na_charmat(SEXP groups_name, size_t cols);

vector<tr2::optional<string>> as_vec_opt_string(CharacterVector& input);
vector<string> get_groups_name(RE2* pattern, int cap_nums);

R_xlen_t re2r_recycling_rule(bool enableWarning, int n, ...);

R_xlen_t vectorize_next(R_xlen_t i, R_xlen_t nrecycle, R_xlen_t n);

typedef unique_ptr<RE2> RE2p;
typedef tr2::optional<RE2p> OptRE2;

void build_regex_vector(SEXP regexp, vector<RE2*>& ptrv);
void build_regex_vector(SEXP regexp, vector<OptRE2*> &ptrv);
void clone_vec_regex(const vector<OptRE2 *>& input, vector<unique_ptr<OptRE2>>& res );

SEXP cpp_detect(CharacterVector &input, vector<OptRE2 *> &ptrv,
                RE2::Anchor anchor_type, size_t nrecycle);
SEXP cpp_detect_parallel(CharacterVector &input, vector<OptRE2 *> &pattern,
                         RE2::Anchor anchor_type, size_t grain_size,
                         size_t nrecycle);

inline size_t getUtf8CharSize(char ch) {
    return ((0xE5000000 >> ((ch >> 3) & 0x1E)) & 3) + 1;
}

inline void bump_count(size_t &coli, size_t &rowi, size_t cols) {
    coli++;
    if (coli == cols) {
        coli = 0;
        rowi++;
    }
}

inline RE2::Anchor get_anchor_type(size_t anchor) {
    if (anchor == 0) {
        return RE2::UNANCHORED;
    } else if (anchor == 1) {
        return RE2::ANCHOR_START;
    } else {
        return RE2::ANCHOR_BOTH;
    }
}

#endif
