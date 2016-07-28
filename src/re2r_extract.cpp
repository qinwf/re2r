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

struct ExtractP : public Worker {
  optstring &input;
  optstring &output;
  vector<OptRE2 *> &tt;

  ExtractP(optstring &input_, optstring &output_, vector<OptRE2 *> &tt_)
      : input(input_), output(output_), tt(tt_) {}

  void operator()(std::size_t begin, std::size_t end) {
    size_t index = begin;

    for (auto x = output.begin() + begin; x != output.begin() + end; x++) {
      auto inputi = input[index % input.size()];
      auto optptr = tt[index % tt.size()];
      index++;

      if (!bool(inputi) || !bool(*optptr)) {
        *x = tr2::nullopt;
        return;
      }
      auto ptr = optptr->value().get();
      StringPiece match;
      StringPiece inputs(inputi.value());
      if (!ptr->Match(inputs, 0, inputs.size(),
                      RE2::UNANCHORED, &match, 1)) {
        *x = tr2::nullopt;
      } else {
        *x = tr2::make_optional(match.as_string());
      }
    }
    return;
  }
};

struct ExtractAllP : public Worker {
  optstring &input;
  vector<tr2::optional<vector<string>>> &output;
  vector<OptRE2 *> &tt;

  ExtractAllP(optstring &input_, vector<tr2::optional<vector<string>>> &output_,
              vector<OptRE2 *> &tt_)
      : input(input_), output(output_), tt(tt_) {}

  void operator()(std::size_t begin, std::size_t end) {
    size_t index = begin;

    for (auto x = output.begin() + begin; x != output.begin() + end; x++) {
      auto inputi = input[index % input.size()];
      auto optptr = tt[index % tt.size()];
      index++;

      if (!bool(inputi) || !bool(*optptr)) {
        *x = tr2::nullopt;
        return;
      }
      RE2 *ptr = optptr->value().get();
      StringPiece match;
      vector<string> res;

      StringPiece str(inputi.value());
      size_t lastIndex = 0;

      while (ptr->Match(str, lastIndex, str.size(),
                        RE2::UNANCHORED, &match, 1)) {
        if (!match.size()) {
          size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
          lastIndex += sym_size;
          continue;
        }
        lastIndex = match.data() - str.data() + match.size();
        res.push_back(match.as_string());
      }

      *x = tr2::make_optional(res);

      return;
    }
  }
};

// [[Rcpp::export]]
SEXP cpp_extract(CharacterVector input, SEXP regexp, bool all, bool parallel,
                 size_t grain_size) {

  vector<OptRE2 *> ptrv;
  build_regex_vector(regexp, ptrv);
  auto nrecycle = re2r_recycling_rule(true, 2, input.size(), ptrv.size());
  SEXP inputx = input;

  if (!parallel || input.size() < grain_size) {

    if (!all) {

      Shield<SEXP> xs(Rf_allocVector(STRSXP, nrecycle));
      SEXP x = xs;

      for (auto it = 0; it != nrecycle; it++) {

        auto rstr = STRING_ELT(inputx, it % input.size());
        auto optptr = ptrv[it % ptrv.size()];
        StringPiece match;
        if (rstr == NA_STRING || !bool(*optptr)) {
          SET_STRING_ELT(x, it, NA_STRING);
          continue;
        }
        auto ptr = optptr->value().get();
        auto r_char = R_CHAR(rstr);
        StringPiece str(r_char);
        auto str_size = str.size();
        size_t lastIndex = 0;
        if (!ptr->Match(str, lastIndex, str_size, RE2::UNANCHORED, &match, 1)) {
          SET_STRING_ELT(x, it, NA_STRING);
        } else {
          string mstring = match.as_string();
          SET_STRING_ELT(x, it,
                         Rf_mkCharLenCE(mstring.c_str(),
                                       match.size(), CE_UTF8));
        }
      }
      return x;
    } else {
      Shield<SEXP> xs(Rf_allocVector(VECSXP, nrecycle));
      SEXP x = xs;

      for (auto it = 0; it != nrecycle; it++) {

        auto optptr = ptrv[it % ptrv.size()];
        auto rstr = STRING_ELT(inputx, it % input.size());

        if (rstr == NA_STRING || !bool(*optptr)) {
          SET_VECTOR_ELT(x, it, CharacterVector());
          continue;
        }
        auto ptr = optptr->value().get();
        StringPiece match;
        StringPiece str(R_CHAR(rstr));
        auto str_size = str.size();
        size_t lastIndex = 0;
        vector<string> res;

        while (
            ptr->Match(str, lastIndex, str_size, RE2::UNANCHORED, &match, 1)) {
          if (!match.size()) {
            size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
            lastIndex += sym_size;
            continue;
          }
          lastIndex = match.data() - str.data() + match.size();
          res.push_back(match.as_string());
        }

        if (res.empty()) {
          SET_VECTOR_ELT(x, it, CharacterVector());
        } else {
          SET_VECTOR_ELT(x, it, Shield<SEXP>(toprotect_vec_string_sexp(res)));
        }
      }
      return x;
    }
  } else { // parallel
    auto inputv = as_vec_opt_string(input);
    if (!all) {
      optstring res(nrecycle);

      ExtractP pobj(inputv, res, ptrv);
      parallelFor(0, nrecycle, pobj, grain_size);
      return toprotect_optstring_sexp(res);
    } else {
      vector<tr2::optional<vector<string>>> res(input.size());

      ExtractAllP pobj(inputv, res, ptrv);
      parallelFor(0, nrecycle, pobj, grain_size);

      Shield<SEXP> xs(Rf_allocVector(VECSXP, input.size()));
      SEXP x = xs;

      R_xlen_t index = 0;

      for (tr2::optional<vector<string>> &resi : res) {
        if (!bool(resi) || resi.value().empty()) {
          SET_VECTOR_ELT(x, index, CharacterVector());
        } else {
          SET_VECTOR_ELT(x, index,
                         Shield<SEXP>(toprotect_vec_string_sexp(resi.value())));
        }
        index++;
      }
      return x;
    }
  }
}
