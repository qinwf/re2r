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

// [[Rcpp::export]]
SEXP cpp_subset(CharacterVector input, SEXP regexp, size_t anchor,
                bool parallel, size_t grain_size, bool omit_na) {
    RE2::Anchor anchor_type = get_anchor_type(anchor);

    vector<OptRE2 *> ptrv;
    build_regex_vector(regexp, ptrv);
    auto nrecycle = re2r_recycling_rule(true, 2, input.size(), ptrv.size());
    LogicalVector res;

    if (!parallel || input.size() < grain_size) {
        res = Shield<SEXP>(cpp_detect(input, ptrv, anchor_type, nrecycle));
    } else {
        res =  Shield<SEXP>(cpp_detect_parallel(input, ptrv, anchor_type, grain_size,
                                   nrecycle));
    }
    size_t total_index = 0;
    vector<size_t> matched;

    if(omit_na){
        for(auto i = res.begin(); i!= res.end(); i++, total_index++){
            if (*i == 1){
                matched.push_back(total_index);
            }
        }

        CharacterVector resx(matched.size());
        size_t index = 0;
        total_index = 0;
        size_t input_size = input.size();

        for(auto i = matched.begin(); i!= matched.end(); i++){
            resx[index] = input[*i % input_size];
            index++;
        }
        return resx;
    }
    else{ // omit_na = FALSE
        for(auto i = res.begin(); i!= res.end(); i++, total_index++){
            if (*i == 1 || *i == NA_LOGICAL){
                matched.push_back(total_index);
            }
        }

        CharacterVector resx(matched.size());
        size_t index = 0;
        total_index = 0;
        size_t input_size = input.size();

        for(auto i = matched.begin(); i!= matched.end(); i++){
            if(res[*i] == 1){
                resx[index] = input[*i % input_size];
                index++;
            }else{
                resx[index] = NA_STRING;
                index++;
            }

        }
        return resx;
    }
}
