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

#define thr(code)                                                              \
  case RE2::ErrorCode::code:                                                   \
    throw code(msg);                                                           \
    break;

void check_compile_error(RE2::ErrorCode code_, const string &msg) {
  switch (code_) {
  case RE2::ErrorCode::NoError:
    return;
    break;
    thr(ErrorInternal) thr(ErrorBadEscape) thr(ErrorBadCharClass)
        thr(ErrorBadCharRange) thr(ErrorMissingBracket) thr(ErrorMissingParen)
            thr(ErrorTrailingBackslash) thr(ErrorRepeatArgument)
                thr(ErrorRepeatSize) thr(ErrorRepeatOp) thr(ErrorBadPerlOp)
                    thr(ErrorBadUTF8) thr(ErrorBadNamedCapture)
                        thr(ErrorPatternTooLarge) default : return;
    break;
  }
}

XPtr<OptRE2> cpp_re2_compile_one(string pattern, bool log_errors_value,
                                 bool utf_8_value, bool posix_syntax_value,
                                 bool case_sensitive_value, bool dot_nl_value,
                                 bool literal_value, bool longest_match_value,
                                 bool never_nl_value, bool never_capture_value,
                                 bool one_line_value, bool perl_classes_value,
                                 bool word_boundary_value,
                                 int64_t max_mem_value) {

  RE2::Options options;

  RE2::Options::Encoding enc_value;
  enc_value = (utf_8_value == true) ? RE2::Options::EncodingUTF8
                                    : RE2::Options::EncodingLatin1;
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

  if (options.posix_syntax() == true) {
    options.set_one_line(one_line_value);
    options.set_perl_classes(perl_classes_value);
    options.set_word_boundary(word_boundary_value);
  }

  XPtr<OptRE2> ptr(

      new OptRE2(tr2::in_place,
                 RE2p(new RE2(
                     StringPiece(pattern.c_str(), (size_t) strlen(pattern.c_str())),
                     options))));

  RE2p &ptrx = (*ptr).value();

  if (!ptrx->ok()) {
    check_compile_error(ptrx->error_code(), ptrx->error_arg());
  }

  return ptr;
}

inline LogicalVector btd(bool input) { return wrap(input); }

// [[Rcpp::export]]
SEXP cpp_re2_compile(CharacterVector input, bool log_errors_value,
                     bool utf_8_value, bool posix_syntax_value,
                     bool case_sensitive_value, bool dot_nl_value,
                     bool literal_value, bool longest_match_value,
                     bool never_nl_value, bool never_capture_value,
                     bool one_line_value, bool perl_classes_value,
                     bool word_boundary_value, int64_t max_mem_value,
                     bool simplify_value) {

  // input size is zero, and return NULL
  if (input.size() == 0) {
    return R_NilValue;
  }

  // return single precompiled pattern
  if (simplify_value && input.size() == 1) {

    // is NA, return NA pattern
    if (*input.begin() == NA_STRING) {
      XPtr<OptRE2> res(new OptRE2(tr2::nullopt));
      Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("re2c"));
      return res;
    } else {

      // this will always be not NA
      Shield<SEXP> res(cpp_re2_compile_one(
          R_CHAR(STRING_ELT(input, 0)), log_errors_value, utf_8_value,
          posix_syntax_value, case_sensitive_value, dot_nl_value, literal_value,
          longest_match_value, never_nl_value, never_capture_value,
          one_line_value, perl_classes_value, word_boundary_value,
          max_mem_value));

      Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("re2c"));
      return res;
    }
  } else {
    Shield<SEXP> resx(Rf_allocVector(VECSXP, input.size()));
    SEXP res = resx;
    SEXP inputx = input;
    auto re2c = Rf_mkString("re2c");
    for (auto it = 0; it != input.size(); it++) {
      auto rstr = STRING_ELT(inputx, it);

      if (rstr == NA_STRING) {
        XPtr<OptRE2> resi(new OptRE2(tr2::nullopt));
        Rf_setAttrib(resi, R_ClassSymbol, re2c);
        SET_VECTOR_ELT(res, it, resi);
        continue;
      }

      auto r_char = R_CHAR(rstr);
      Shield<SEXP> resi(cpp_re2_compile_one(
          r_char, log_errors_value, utf_8_value, posix_syntax_value,
          case_sensitive_value, dot_nl_value, literal_value,
          longest_match_value, never_nl_value, never_capture_value,
          one_line_value, perl_classes_value, word_boundary_value,
          max_mem_value));
      Rf_setAttrib(resi, R_ClassSymbol, re2c);
      SET_VECTOR_ELT(res, it, resi);
    }
    return res;
  }
}
