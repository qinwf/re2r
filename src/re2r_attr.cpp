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
SEXP cpp_get_program_fanout(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {
    map<int, int> res;
    ptr->value()->ProgramFanout(&res);
    return (wrap(res));
  } else {
    return R_NilValue;
  }
}

// [[Rcpp::export]]
SEXP cpp_regex_to_string(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {

    string mstring = regexp->value()->Regexp()->ToString();
    Shield<SEXP> res(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(res, 0, Rf_mkCharLenCE(mstring.c_str(),
                                          strlen(mstring.c_str()), CE_UTF8));
    return res;
  } else {
    return CharacterVector(NA_STRING);
  }
}

// [[Rcpp::export]]
LogicalVector cpp_regex_mimicsPCRE(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {
    auto ptri = regexp->value()->Regexp();
    return wrap(ptri->MimicsPCRE());
  } else {
    return NA_LOGICAL;
  }
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
//'
//' get_number_of_groups(re2("((?P<a>123)(12))"))
//'
//' # uncaptured groups
//' get_number_of_groups(re2("(?:(?:(?:(?:(?:.)?){100})*)+)"))
//' @export
// [[Rcpp::export]]
int get_number_of_groups(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {
    return ptr->value()->NumberOfCapturingGroups();
  } else {
    return NA_INTEGER;
  }
}

// [[Rcpp::export]]
SEXP cpp_get_named_groups(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {
    return wrap(get_groups_name(ptr->value().get(),
                                ptr->value()->NumberOfCapturingGroups()));
  } else {
    return CharacterVector(NA_STRING);
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
//' get_expression_size(re2("1"))
//' get_expression_size(re2("(1)"))
//' get_expression_size(re2("(?:(?:(?:(?:(?:.)?){100})*)+)"))
//' @export
// [[Rcpp::export]]
int get_expression_size(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {
    return ptr->value()->ProgramSize();
  } else {
    return NA_INTEGER;
  }
}

//' Check NA pattern
//'
//' Returns whether a pre-compiled regular expression is NA.
//'
//' @param regexp a pre-compiled regular expression
//' @return boolean
//' @examples
//' is_re2c_na(re2(NA))
//' @export
// [[Rcpp::export]]
LogicalVector is_re2c_na(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  LogicalVector res(1);
  if (bool(*ptr)) {
    res[0] = Rboolean::FALSE;
    return res;
  } else {
    res[0] = Rboolean::TRUE;
    return res;
  }
}

// [[Rcpp::export]]
SEXP cpp_get_pattern(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {
    Shield<SEXP> res(Rf_allocVector(STRSXP, 1));
    string ress = ptr->value()->pattern();
    SET_STRING_ELT(res, 0,
                   Rf_mkCharLenCE(ress.c_str(), strlen(ress.c_str()), CE_UTF8));
    return res;
  } else {
    return CharacterVector(NA_STRING);
  }
}

//' Get options of a pre-compiled regular expression
//'
//' Returns options of a pre-compiled regular expression
//'
//' @param regexp a pre-compiled regular expression
//' @return an R list
//' @examples
//' get_options(re2("test"))
//' @export
// [[Rcpp::export]]
SEXP get_options(XPtr<OptRE2> &regexp) {
  OptRE2 *ptr = regexp.get();
  if (bool(*ptr)) {
    List res(13);
    CharacterVector name = CharacterVector::create(
        "utf_8", "case_sensitive", "posix_syntax", "dot_nl", "literal",
        "longest_match", "never_nl", "never_capture", "one_line",
        "perl_classes", "word_boundary", "log_error", "max_mem");
    auto options = ptr->value()->options();
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
  } else {
    return List::create(R_NilValue);
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
