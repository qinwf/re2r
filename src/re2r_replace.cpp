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

struct ReplaceP : public Worker {
  optstring &input;
  optstring &res_replace;
  vector<OptRE2 *> &tt;
  optstring &rewrite;

  ReplaceP(optstring &input_, vector<OptRE2 *> &tt_, optstring &rewrite_,
           optstring &res_replace_)
      : input(input_), res_replace(res_replace_), tt(tt_), rewrite(rewrite_) {}

  void operator()(std::size_t begin, std::size_t end) {
    size_t index = begin;
    std::for_each(res_replace.begin() + begin, res_replace.begin() + end,
                  [this, &index](tr2::optional<string> &x) {
                    tr2::optional<string>& strix = input[index % input.size()];
                    OptRE2 *optpattern = tt[index % tt.size()];
                    if (!bool(strix) || !bool(*optpattern)) {
                      x = tr2::nullopt;
                      index++;
                      return;
                    }

                    x = strix;
                    auto pattern = optpattern->value().get();
                    auto rewritei = rewrite[index % rewrite.size()];

                    if (!bool(rewritei)) {
                      if (pattern->Match(x.value(), 0,
                                         strlen(x.value().c_str()),
                                         RE2::UNANCHORED, nullptr, 0)) {
                        x = tr2::nullopt;
                      }
                      index++;
                      return;
                    }

                    pattern->Replace(&x.value(), *pattern, rewritei.value());
                    index++;
                    return;
                  });
  }
};

struct ReplaceGlobalP : public Worker {
  optstring &input;
  optstring &res_replace;
  vector<size_t> &count;
  vector<OptRE2 *> &tt;
  optstring &rewrite;

  ReplaceGlobalP(optstring &input_, vector<size_t> &count_,
                 vector<OptRE2 *> &tt_, optstring &rewrite_,
                 optstring &res_replace_)
      : input(input_), res_replace(res_replace_), count(count_), tt(tt_),
        rewrite(rewrite_) {}

  void operator()(std::size_t begin, std::size_t end) {
    size_t index = begin;
    std::transform(
        res_replace.begin() + begin, res_replace.begin() + end,
        count.begin() + begin, [this, &index](tr2::optional<string> &x) {
          auto optptr = tt[index % tt.size()];
          auto rewritei = rewrite[index % rewrite.size()];
          tr2::optional<string>& strix = input[index % input.size()];
          if (!bool(strix) || !bool(*optptr)) {
            x = tr2::nullopt;
            index++;
            return 0;
          }
          x = strix;
          auto ptr = optptr->value().get();

          if (!bool(rewritei)) {
              index++;
            if (ptr->Match(x.value(), 0, strlen(x.value().c_str()),
                           RE2::UNANCHORED, nullptr, 0)) {
              x = tr2::nullopt;
              return 1;
            } else{
              return 0;
            }


          }
          index++;
          return ptr->GlobalReplace(&x.value(), *ptr, rewritei.value());
        });
  }
};

// [[Rcpp::export]]
SEXP cpp_replace(CharacterVector input, SEXP regexp, CharacterVector rewrite_,
                 bool global_, bool parallel, size_t grain_size) {
  string errmsg;
  auto inputv = as_vec_opt_string(input);
  auto rewrite = as_vec_opt_string(rewrite_);
  vector<OptRE2 *> ptrv;
  build_regex_vector(regexp, ptrv);
  auto nrecycle =
      re2r_recycling_rule(true, 3, input.size(), ptrv.size(), rewrite.size());

  for (auto i = 0; i != nrecycle; i++) {
    auto optptr = ptrv[i % ptrv.size()];
    auto rewritei = rewrite[i % rewrite.size()];
    if (bool(rewritei) && bool(*optptr)) {
      if (!optptr->value()->CheckRewriteString(rewritei.value(), &errmsg)) {
        throw ErrorRewriteString(errmsg);
      }
    }
  }

  if (!global_) {

    vector<tr2::optional<string>> replace_res;
    replace_res.reserve(nrecycle);

    if (!parallel || nrecycle < grain_size) {
      for (auto i = 0; i != nrecycle; i++) {
        auto optptr = ptrv[i % ptrv.size()];
        auto rewritei = rewrite[i % rewrite.size()];
        tr2::optional<string> & strix = inputv[i % input.size()];

        if (!bool(strix) || !bool(*optptr)) {
          replace_res.push_back(tr2::nullopt);
          continue;
        }
        replace_res.push_back(strix.value());
        tr2::optional<string> &stri = replace_res.back();
        auto ptr = optptr->value().get();

        if (!bool(rewritei)) {
          if (ptr->Match(stri.value(), 0, strlen(stri.value().c_str()),
                         RE2::UNANCHORED, nullptr, 0)) {
            stri = tr2::nullopt;
          }
          continue;
        }
        ptr->Replace(&stri.value(), *ptr, rewritei.value());
      }
      CharacterVector res(toprotect_optstring_sexp(replace_res));
      return res;
    } else {
      optstring replace_res(nrecycle);
      ReplaceP pobj(inputv, ptrv, rewrite, replace_res);
      parallelFor(0, replace_res.size(), pobj, grain_size);
      CharacterVector res(toprotect_optstring_sexp(replace_res));
      return res;
    }
  } else {
    vector<size_t> count;
    count.reserve(nrecycle);

    if (!parallel || input.size() < grain_size) {

      vector<tr2::optional<string>> replace_res;
      replace_res.reserve(nrecycle);

      for (auto i = 0; i != nrecycle; i++) {
        auto optptr = ptrv[i % ptrv.size()];
        auto rewritei = rewrite[i % rewrite.size()];

        tr2::optional<string> &strix = inputv[i % input.size()];

        if (!bool(strix) || !bool(*optptr)) {
          replace_res.push_back(tr2::nullopt);
          count.push_back(0);
          continue;
        }
        replace_res.push_back(strix);
        tr2::optional<string> & stri = replace_res.back();
        auto ptr = optptr->value().get();

        if (!bool(rewritei)) {

          if (ptr->Match(stri.value(), 0, strlen(stri.value().c_str()),
                         RE2::UNANCHORED, nullptr, 0)) {
            stri = tr2::nullopt;
            count.push_back(1);
          }else{
            count.push_back(0);
          }
          continue;
        }
        count.push_back(
            ptr->GlobalReplace(&stri.value(), *ptr, rewritei.value()));
      }
      CharacterVector res(toprotect_optstring_sexp(replace_res));
      res.attr("count") = count;
      return res;
    } else {
      vector<tr2::optional<string>> replace_res(nrecycle);
      count.resize(input.size());
      ReplaceGlobalP pobj(inputv, count, ptrv, rewrite, replace_res);
      parallelFor(0, replace_res.size(), pobj, grain_size);

      CharacterVector res(toprotect_optstring_sexp(replace_res));
      res.attr("count") = count;
      return res;
    }
  }
}
