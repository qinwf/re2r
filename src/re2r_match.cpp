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

#include <cstddef>

RE2::Anchor get_anchor_type(const string& anchor){
    if (anchor == "none") {
        return RE2::UNANCHORED;
    } else if (anchor == "start") {
        return RE2::ANCHOR_START;
    } else if (anchor == "both") {
        return RE2::ANCHOR_BOTH;
    }
    throw ErrorAnchorType(anchor);
}

// [[Rcpp::export]]
SEXP cpp_match(XPtr<RE2>&     pattern,
                  vector<string>& input,
                  bool value,
                  string& anchor,
                  bool all){
    RE2::Anchor anchor_type = get_anchor_type(anchor);

    if (value == false){
        vector<bool> res;
        res.reserve(input.size());
        for(const string& ind : input){
            res.push_back(pattern->Match(ind,0,(int) ind.length(),
                                         anchor_type, nullptr, 0));
        }
        return wrap(res);
    } else{
            // no capture group, return CharacterVector, like grep(value = T)
        if (pattern->NumberOfCapturingGroups() == 0){
            vector<string> res;
            res.reserve(input.size());
            for(const string& ind : input){
                if(pattern->Match(ind,0,(int) ind.length(),
                                    anchor_type, nullptr, 0)){
                    res.push_back(ind);
                };
                return(wrap(ind));
            }
            return wrap(res);
        }

        // at least one capture group, return data.frame

        if (all == false) {

            return wrap(1);
        } else {
            return wrap(1);
        }

    }
    throw ErrorInternal("unreachable cpp_match");
}
