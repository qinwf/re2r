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

#include <sstream>

template <typename T>
string NumberToString ( T Number )
{
    ostringstream ss;
    ss << "?";
    ss << Number;
    return ss.str();
}

map<int, string> get_groups_name(XPtr<RE2>& pattern, int cap_nums){
    auto groups_name = pattern->CapturingGroupNames();

    vector<int> alls;
    alls.reserve(cap_nums);
    int cnt = 1;
    while(cnt <= cap_nums){
        alls.push_back(cnt);
        cnt+=1;
    }

    vector<int> nums;
    nums.reserve(cap_nums);

    vector<string> cap_names;
    cap_names.reserve(cap_nums);

    for(auto it = groups_name.begin(); it != groups_name.end(); ++it) {
        nums.push_back(it->first);
    }

    vector<int> diff_nums(alls.size()+nums.size());

    auto diff_res = set_difference(alls.begin(),
                                   alls.end(),
                                   nums.begin(),
                                   nums.end(),
                                   diff_nums.begin());
    diff_nums.resize(diff_res-diff_nums.begin());

    for(auto ind : diff_nums) {
        groups_name.insert(make_pair(ind, NumberToString(ind)));
    }
    return groups_name;
}


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
        auto cap_nums = pattern->NumberOfCapturingGroups();
            // no capture group, return CharacterVector, like grep(value = T)
        if ( cap_nums == 0){
            vector<string> res;
            res.reserve(input.size());
            for(const string& ind : input){
                if(pattern->Match(ind,0,(int) ind.length(),
                                    anchor_type, nullptr, 0)){
                    res.push_back(ind);
                }
            }
            return wrap(res);
        }

        // at least one capture group, return data.frame
        map<int, string> groups_name = get_groups_name(pattern, cap_nums);

        if (all == false) {
            return wrap(groups_name);
        } else {
            return wrap(groups_name);
        }

    }
    throw ErrorInternal("unreachable cpp_match");
}
