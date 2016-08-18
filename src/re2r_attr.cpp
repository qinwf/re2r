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

template <typename T> inline string numbertostring(T Number) {
  ostringstream ss;
  ss << Number;
  return ss.str();
}

// [[Rcpp::export]]
SEXP cpp_get_groups(SEXP regexp) {
    INIT_ptr if (bool(*ptr)) { return wrap(ptr->value()->NumberOfCapturingGroups()); }
    else {
       return IntegerVector::create(NA_INTEGER);
    }
}

// [[Rcpp::export]]
SEXP cpp_get_program_fanout(SEXP regexp) {
  INIT_ptr if (bool(*ptr)) {
    map<int, int> res;
    ptr->value()->ProgramFanout(&res);
    return (wrap(res));
  }
  else {
    return R_NilValue;
  }
}

// [[Rcpp::export]]
SEXP cpp_regex_to_string(SEXP regexp) {
  INIT_ptr if (bool(*ptr)) {

    string mstring = ptr->value()->Regexp()->ToString();
    Shield<SEXP> res(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(res, 0, Rf_mkCharLenCE(mstring.c_str(),
                                          strlen(mstring.c_str()), CE_UTF8));
    return res;
  }
  else {
    return CharacterVector(NA_STRING);
  }
}

// [[Rcpp::export]]
LogicalVector cpp_regex_mimicsPCRE(SEXP regexp) {
  INIT_ptr if (bool(*ptr)) {
    auto ptri = ptr->value()->Regexp();
    return wrap(ptri->MimicsPCRE());
  }
  else {
    return NA_LOGICAL;
  }
}


// [[Rcpp::export]]
SEXP cpp_get_named_groups(SEXP regexp) {
  INIT_ptr if (bool(*ptr)) {
    return wrap(get_groups_name(ptr->value().get(),
                                ptr->value()->NumberOfCapturingGroups()+1));
  }
  else {
    return CharacterVector::create(NA_STRING);
  }
}


// [[Rcpp::export]]
int cpp_get_expression_size(SEXP regexp) {
  INIT_ptr if (bool(*ptr)) { return ptr->value()->ProgramSize(); }
  else {
    return NA_INTEGER;
  }
}

// [[Rcpp::export]]
LogicalVector cpp_is_re2c_na(SEXP regexp) {
  INIT_ptr LogicalVector res(1);
  if (bool(*ptr)) {
    res[0] = false;
    return res;
  } else {
    res[0] = true;
    return res;
  }
}

// [[Rcpp::export]]
SEXP cpp_get_pattern(SEXP regexp) {
  INIT_ptr if (bool(*ptr)) {
    Shield<SEXP> res(Rf_allocVector(STRSXP, 1));
    string ress = ptr->value()->pattern();
    SET_STRING_ELT(res, 0,
                   Rf_mkCharLenCE(ress.c_str(), strlen(ress.c_str()), CE_UTF8));
    return res;
  }
  else {
    return CharacterVector(NA_STRING);
  }
}


// [[Rcpp::export]]
SEXP cpp_get_options(SEXP regexp) {
  INIT_ptr if (bool(*ptr)) {
    List res(13);
    CharacterVector name = CharacterVector::create(
        "utf_8", "case_sensitive", "posix_syntax", "dot_nl", "literal",
        "longest_match", "never_nl", "never_capture", "one_line",
        "perl_classes", "word_boundary", "log_error", "max_mem");
    const RE2::Options& options = ptr->value()->options();
    res[0] = options.utf8();
    res[1] = options.case_sensitive();
    res[2] = options.posix_syntax();
    res[3] = options.dot_nl();
    res[4] = options.literal();
    res[5] = options.longest_match();
    res[6] = options.never_nl();
    res[7] = options.never_capture();
    res[8] = options.one_line();
    res[9] = options.perl_classes();
    res[10] = options.word_boundary();
    res[11] = options.log_errors();
    res[12] = options.max_mem();
    res.attr("names") = name;
    return res;
  }
  else {
    return List(0);
  }
}

struct QuoteMetaP : public Worker {
  // source
  optstring &input;
  // destination
  optstring &output;

  // initialize with source and destination
  QuoteMetaP(optstring &input_, optstring &output_)
      : input(input_), output(output_) {}

  // the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    RE2 tt("");
    std::transform(
        input.begin() + begin, input.begin() + end, output.begin() + begin,
        [this, &tt](tr2::optional<string> &x) -> tr2::optional<string> {
          if (!bool(x)) {
            return tr2::nullopt;
          }
          return tr2::make_optional(tt.QuoteMeta(x.value()));
        });
  }
};

// [[Rcpp::export]]
SEXP cpp_quote_meta(CharacterVector input, bool parallel, size_t grain_size) {

  if (!parallel || input.size() < grain_size) {
    SEXP inputx = input;
    Shield<SEXP> ress(Rf_allocVector(STRSXP, input.size()));
    SEXP res = ress;
    RE2 tt(""); // break on windows without tt
    for (auto it = 0; it != input.size(); it++) {
      auto rstr = STRING_ELT(inputx, it);
      if (rstr == NA_STRING) {
        SET_STRING_ELT(res, it, NA_STRING);
        continue;
      }

      auto resi = tt.QuoteMeta(R_CHAR(rstr));
      SET_STRING_ELT(
          res, it, Rf_mkCharLenCE(resi.c_str(), strlen(resi.c_str()), CE_UTF8));
    }

    return res;
  } else {
    optstring res(input.size());
    auto inputv = as_vec_opt_string(input);
    QuoteMetaP pobj(inputv, res);
    parallelFor(0, input.size(), pobj, grain_size);
    return toprotect_optstring_sexp(res);
  }
}


