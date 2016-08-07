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

struct CountP : public Worker {
    optstring &input;
    vector<tr2::optional<R_xlen_t>> &output;
    vector<OptRE2 *> &tt;
    RE2::Anchor& anchor_type;

    CountP(optstring &input_, vector<tr2::optional<R_xlen_t>> &output_,
                vector<OptRE2 *> &tt_,RE2::Anchor& anchor_type_)
        : input(input_), output(output_), tt(tt_), anchor_type(anchor_type_) {}

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
            R_xlen_t resi = 0;
            StringPiece str(inputi.value());
            size_t lastIndex = 0;

            while (ptr->Match(str, lastIndex, str.size(),
                              anchor_type, &match, 1)) {
                resi++;
                if (!match.size()) {
                    size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
                    lastIndex += sym_size;
                    continue;
                }
                lastIndex = match.data() - str.data() + match.size();
            }
            *x = resi;
            return;
        }
    }
};



// [[Rcpp::export]]
SEXP cpp_count(CharacterVector input, SEXP regexp, size_t anchor, bool parallel,
                 size_t grain_size) {
    RE2::Anchor anchor_type = get_anchor_type(anchor);

    vector<OptRE2 *> ptrv;
    build_regex_vector(regexp, ptrv);
    auto nrecycle = re2r_recycling_rule(true, 2, input.size(), ptrv.size());
    SEXP inputx = input;

    if (!parallel || input.size() < grain_size) {

            Shield<SEXP> xs(Rf_allocVector(INTSXP, nrecycle));
            auto x = INTEGER(xs);

            for (auto it = 0; it != nrecycle; it++) {

                auto optptr = ptrv[it % ptrv.size()];
                auto rstr = STRING_ELT(inputx, it % input.size());

                if (rstr == NA_STRING || !bool(*optptr)) {
                    x[it] = NA_INTEGER;
                    continue;
                }
                auto ptr = optptr->value().get();
                StringPiece match;
                StringPiece str(R_CHAR(rstr));
                auto str_size = str.size();
                size_t lastIndex = 0;
                R_xlen_t res = 0;
                while (
                        ptr->Match(str, lastIndex, str_size, anchor_type, &match, 1)) {
                    res++;
                    if (!match.size()) {
                        size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
                        lastIndex += sym_size;
                        continue;
                    }
                    lastIndex = match.data() - str.data() + match.size();
                }

                x[it] = res;
            }
            return wrap(xs);
        }
     else { // parallel
            auto inputv = as_vec_opt_string(input);
            vector<tr2::optional<R_xlen_t>> res(nrecycle);

            CountP pobj(inputv, res, ptrv,anchor_type);
            parallelFor(0, nrecycle, pobj, grain_size);

            Shield<SEXP> xs(Rf_allocVector(INTSXP, nrecycle));
            auto x = INTEGER(xs);
            R_xlen_t index = 0;

            for (tr2::optional<R_xlen_t> &resi : res) {
                if (!bool(resi)) {
                    x[index] = NA_INTEGER;
                } else {
                    x[index] = resi.value();
                }
                index++;
            }
            return xs;
        }

}
