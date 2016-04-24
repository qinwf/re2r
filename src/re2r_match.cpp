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
#include "../inst/include/optional.hpp"

#include <cstddef>

#include <sstream>
#include <memory>

namespace tr2 = std::experimental;

typedef vector<tr2::optional<string>> optstring;

#define RE2R_STATIC_SIZE 10

template <typename T>
inline string NumberToString ( T Number )
{
    ostringstream ss;
    ss << "?";
    ss << Number;
    return ss.str();
}

template <typename T>
inline string numbertostring ( T Number )
{
    ostringstream ss;
    ss << Number;
    return ss.str();
}

map<int,string> get_groups_name(XPtr<RE2>& pattern, int cap_nums){
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

void fill_all_res(string& times_n,
                  int cap_nums,
                  StringPiece* piece,
                  optstring& res, size_t cnt,bool matched){
    auto all_na = true;

    // don't get all na
    if(cnt > 1 && matched ==true ){
        for(auto it = 0; it != cap_nums; ++it) {
            if((piece[it]).data() != NULL){
                    all_na = false;
                    break;
            }
        }
        if (all_na) return;
    }

    res.push_back(tr2::make_optional(times_n));

    if(matched){
        for(auto it = 0; it != cap_nums; ++it) {
            if((piece[it]).data() != NULL){
                res.push_back(tr2::make_optional(piece[it].as_string())) ;
            } else{
                res.push_back(tr2::nullopt);
            }
        }
    }else{
        for(auto it = 0; it != cap_nums; ++it) {
            res.push_back(tr2::nullopt);
        }
    }
}


void fill_res(int cap_nums,
              StringPiece* piece,
              CharacterVector::iterator& res, bool matched){
    if(matched){
        for(auto it = 0; it != cap_nums; ++it) {
            if((piece[it]).data() != NULL){
                *res = piece[it].as_string();
            } else{
                *res = NA_STRING;
            }
            res +=1;
        }
    }else{
        for(auto it = 0; it != cap_nums; ++it) {
            *res = NA_STRING;
            res +=1;
        }
    }

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
SEXP cpp_match(vector<string>& input,
               XPtr<RE2>& pattern,
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
        // bool return, the fastest one
    } else{
        auto cap_nums = pattern->NumberOfCapturingGroups();

        if ( cap_nums == 0){
            CharacterVector res(input.size());
            auto ip = input.begin();
            for(auto it = res.begin(); it!= res.end(); it++){
                if(pattern->Match(*ip,0,(int) ip->length(),
                                  anchor_type, nullptr, 0)){
                    *it = *ip;
                } else {
                    *it = NA_STRING;
                }
                ip++;
            }
            res.attr("dim") = Dimension(input.size(),1);
            CharacterMatrix mat_res = wrap(res);

            colnames(mat_res) = CharacterVector::create("?nocapture");
            return wrap(mat_res);
            // no capture group return
        }

        // at least one capture group, return a data.frame

        // set up the args and stringpiece
        map<int,string> g_numbers_names = get_groups_name(pattern, cap_nums);
        vector<string> groups_name;
        vector<int> groups_number;



        // static when the number of capture group is smaller than 10
        RE2::Arg* args_static[RE2R_STATIC_SIZE];
        RE2::Arg  argv_static[RE2R_STATIC_SIZE];
        StringPiece piece_static[RE2R_STATIC_SIZE];

        // dynamic when the number of capture group is bigger than 10
        // We use exception, it will be better to use unique_ptr instead of raw ptr.
        unique_ptr<RE2::Arg[]> argv;
        unique_ptr<RE2::Arg*[]> args;
        unique_ptr<StringPiece[]> piece;
        // pointer to used
        RE2::Arg** args_ptr;
        StringPiece* piece_ptr;
        RE2::Arg* argv_ptr;

        // when we have a small number of capture groups,
        // we do not need to used heap
        if(cap_nums <= RE2R_STATIC_SIZE){ // RE2R_STATIC_SIZE = 10
            args_ptr = args_static;
            argv_ptr = argv_static;
            piece_ptr = piece_static;
        } else{
            argv =  unique_ptr<RE2::Arg[]>(new RE2::Arg[cap_nums]);
            args =  unique_ptr<RE2::Arg*[]>(new RE2::Arg*[cap_nums]);
            piece = unique_ptr<StringPiece[]>(new StringPiece[cap_nums]);
            piece_ptr = piece.get();
            args_ptr = args.get();
            argv_ptr = argv.get();
        };

        // It we do not manually delete this, it will be safe:
        // args_ptr， argv_ptr, piece_ptr
        // below we can only use these three ptr
        //
        for (int nn = 0; nn != cap_nums; nn++){
            args_ptr[nn] = &argv_ptr[nn];
            argv_ptr[nn] = &piece_ptr[nn];
        }
        // each string get a group of result
        if (all == false) {

            groups_name.reserve(g_numbers_names.size());
            groups_number.reserve(g_numbers_names.size());
            for(auto it = g_numbers_names.begin(); it!= g_numbers_names.end(); it++) {
                groups_number.push_back(it->first);
                groups_name.push_back(it->second);
            }

            CharacterVector res(cap_nums * input.size()); // will be constructed as Matrix
            auto resi = res.begin();

            switch(anchor_type){
            case RE2::UNANCHORED:
                for(const string& ind : input){
                    for(int pn = 0; pn!=cap_nums; pn++) piece_ptr[pn].clear();

                    fill_res(cap_nums,
                             piece_ptr, resi,
                             RE2::PartialMatchN(ind, *pattern, args_ptr, cap_nums));
                }
                break;
            default:
                for(const string& ind : input){
                    for(int pn = 0; pn!=cap_nums; pn++) piece_ptr[pn].clear();

                    fill_res(cap_nums,
                             piece_ptr, resi,
                             RE2::FullMatchN(ind, *pattern, args_ptr, cap_nums));
                    break;
                }
            }


            // generate CharacterMatrix
            res.attr("dim") = Dimension(cap_nums,input.size());
            CharacterMatrix mat_res = wrap(res);
            CharacterMatrix t_mat_res = transpose(mat_res);
            colnames(t_mat_res) = wrap(groups_name);
            return wrap(t_mat_res);

        } else { // all == true

            // each string get at least one group of result
            groups_name.reserve(g_numbers_names.size()+1);
            groups_number.reserve(g_numbers_names.size()+1);
            groups_name.push_back("!n");
            groups_number.push_back(0);

            for(auto it = g_numbers_names.begin(); it!= g_numbers_names.end(); it++) {
                groups_number.push_back(it->first);
                groups_name.push_back(it->second);
            }

            optstring optres;
            // for each input string, get a !n label.
            size_t times_n = 1;

            //  FIXME Duplicated Code
            if (anchor_type == RE2::UNANCHORED){
                for(const string& ind : input){
                    StringPiece todo_str(ind);    // Wrap a StringPiece around it
                    StringPiece tmp_piece = StringPiece(todo_str.data(), todo_str.length());
                    for(int pn = 0; pn!=cap_nums; pn++) piece_ptr[pn].clear();
                    size_t cnt = 0;
                    while (RE2::FindAndConsumeN(&todo_str, *pattern, args_ptr, cap_nums)) {
                        cnt+=1;
                        string numstring = numbertostring(times_n);
                        fill_all_res(numstring, cap_nums, piece_ptr, optres, cnt, true);

                        for(int pn = 0; pn!=cap_nums; pn++) piece_ptr[pn].clear();

                        // Note that if the
                        // regular expression matches an empty string, input will advance
                        // by 0 bytes.  If the regular expression being used might match
                        // an empty string, the loop body must check for this case and either
                        // advance the string or break out of the loop.
                        //
                        if(todo_str.length() == 0) break; // end of search for this string

                        if((todo_str.data() == tmp_piece.data()) &&
                           (todo_str.length() == tmp_piece.length()) &&
                           (todo_str.length() !=0) ){
                            todo_str.remove_prefix(1);
                        }

                        // update tmp_piece
                        tmp_piece = StringPiece(todo_str.data(), todo_str.length());

                        // try next place
                    }   // while
                    if(cnt == 0){ // no one match, all NA return
                        string numstring = numbertostring(times_n);
                        fill_all_res(numstring, cap_nums, piece_ptr, optres, cnt, false);
                    }
                    times_n+=1; //bump times_n !n
                }}else{
                    for(const string& ind : input){
                        StringPiece todo_str(ind);    // Wrap a StringPiece around it
                        StringPiece tmp_piece = StringPiece(todo_str.data(), todo_str.length());
                        for(int pn = 0; pn!=cap_nums; pn++) piece_ptr[pn].clear();
                        size_t cnt = 0;
                        while (RE2::ConsumeN(&todo_str, *pattern, args_ptr, cap_nums)) {
                            cnt+=1;
                            string numstring = numbertostring(times_n);
                            fill_all_res(numstring, cap_nums, piece_ptr, optres, cnt, true);

                            for(int pn = 0; pn!=cap_nums; pn++) piece_ptr[pn].clear();

                            // Note that if the
                            // regular expression matches an empty string, input will advance
                            // by 0 bytes.  If the regular expression being used might match
                            // an empty string, the loop body must check for this case and either
                            // advance the string or break out of the loop.
                            //
                            if(todo_str.length() == 0) break; // end of search for this string

                            if((todo_str.data() == tmp_piece.data()) &&
                               (todo_str.length() == tmp_piece.length()) &&
                               (todo_str.length() !=0) ){
                                todo_str.remove_prefix(1);
                            }

                            // update tmp_piece
                            tmp_piece = StringPiece(todo_str.data(), todo_str.length());

                            // advanced try next place
                        }   // else while
                        if(cnt == 0){ // no one match, all NA return
                            string numstring = numbertostring(times_n);
                            fill_all_res(numstring, cap_nums, piece_ptr, optres, cnt, false);
                        }
                        times_n+=1; //bump times_n !n
                    }
                } // end else
                CharacterVector res(optres.size());

                auto it = res.begin();
                for(auto dd : optres){
                    if (bool(dd)) {
                        *it = dd.value();
                    } else{
                        *it = NA_STRING;
                    }
                    it++;
                }
                // generate CharacterMatrix
                res.attr("dim") = Dimension(groups_name.size(), res.size() / groups_name.size());
                CharacterMatrix mat_res = wrap(res);
                CharacterMatrix t_mat_res = transpose(mat_res);
                colnames(t_mat_res) = wrap(groups_name);
                return wrap(t_mat_res);

        }

        // unique_ptr go out of scrope
    }
    throw ErrorInternal("unreachable cpp_match");
}

